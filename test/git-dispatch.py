#!/usr/bin/env python3
"""Bounded routing, repository boundaries, and real ngit/Git comparisons.

Run with routing, boundaries, e2e, or all (default). No persistent fixtures.
"""
import base64
import json
import os
from pathlib import Path
import runpy
import shutil
import signal
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

ROOT = Path(__file__).resolve().parents[1]
DISPATCH = ROOT / "bin/ngit-git"
API = runpy.run_path(str(DISPATCH))
GIT = shutil.which("git")
MAGIT = ["--no-pager", "--literal-pathspecs", "-c", "core.preloadindex=true",
         "-c", "log.showSignature=false", "-c", "color.ui=false", "-c", "color.diff=false"]
MODE = sys.argv.pop(1) if len(sys.argv) > 1 else "all"


def run(command, cwd, env, data=b""):
    process = subprocess.Popen(command, cwd=cwd, env=env, stdin=subprocess.PIPE,
                               stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                               start_new_session=True)
    try:
        out, err = process.communicate(data, timeout=40)
    except subprocess.TimeoutExpired:
        os.killpg(process.pid, signal.SIGKILL)
        process.communicate()
        raise
    return process.returncode, out, err, process.pid


class Fixture(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="ngit-dispatch-")
        self.addCleanup(self.temp.cleanup)
        self.base = Path(self.temp.name)
        self.repo = self.base / "repo"
        self.repo.mkdir()
        self.env = {k: v for k, v in os.environ.items()
                    if not k.startswith(("GIT_", "NGIT_"))}
        self.env.update(HOME=str(self.base), XDG_CONFIG_HOME=str(self.base / "xdg"),
                        GIT_CONFIG_NOSYSTEM="1", GIT_CONFIG_GLOBAL=os.devnull,
                        GIT_AUTHOR_DATE="2001-01-01T00:00:00+0000",
                        GIT_COMMITTER_DATE="2001-01-01T00:00:00+0000",
                        LC_ALL="C", NGIT_REAL_GIT=GIT,
                        NGIT_GIT_TRACE=str(self.base / "trace"))
        self.git("init", "-q", "--object-format=sha1")
        self.git("config", "user.name", "Dispatch Test")
        self.git("config", "user.email", "dispatch@example.invalid")
        (self.repo / "file").write_text("one\n")
        self.git("add", "file")
        self.git("commit", "-qm", "initial")

    def git(self, *args):
        result = run([GIT, *args], self.repo, self.env)
        self.assertEqual(result[0], 0, result[2])
        return result[1]

    def dispatch(self, args, env=None, data=b""):
        return run([str(DISPATCH), *args], self.repo, env or self.env, data)

    def backend(self):
        return json.loads((self.base / "trace").read_text().splitlines()[-1])["backend"]

    def fake(self):
        script = '''#!/usr/bin/env python3
import base64,json,os,subprocess,sys
a=sys.argv[1:]; tail=a[:]
while tail and tail[0].startswith('-'):
 if tail[0] in ('-C','-c'): tail=tail[2:]
 else: tail=tail[1:]
probes=[['config','--null','--list'],['ls-files','--stage','-v','-z'],['ls-files','--debug','-z'],['ls-files','--eol','--cached','--others','--exclude-standard','-z'],['for-each-ref','--count=1','--format=%(refname)','refs/replace'],['var','GIT_ATTR_SYSTEM'],['var','GIT_ATTR_GLOBAL']]
if os.path.basename(sys.argv[0])=='git-fake' and (tail in probes or tail[:2]==['rev-parse','--path-format=absolute']):
 os.execvpe(os.environ['TEST_REAL_GIT'],[os.environ['TEST_REAL_GIT'],*a],os.environ)
record={'args':a,'stdin':base64.b64encode(sys.stdin.buffer.read()).decode(),'pid':os.getpid(),'backend':os.path.basename(sys.argv[0])}
with open(os.environ['TEST_RECORD'],'a') as f: f.write(json.dumps(record)+'\\n')
sys.exit(int(os.environ.get('TEST_EXIT','0')))
'''
        for name in ("git-fake", "ngit-fake"):
            path = self.base / name
            path.write_text(script)
            path.chmod(0o755)
        self.env.update(NGIT_REAL_GIT=str(self.base / "git-fake"),
                        NGIT_COMMAND=str(self.base / "ngit-fake"),
                        TEST_REAL_GIT=GIT, TEST_RECORD=str(self.base / "record"))


@unittest.skipUnless(MODE in ("all", "routing"), "mode")
class Routing(Fixture):
    def test_supported_grammar(self):
        cases = [["status"], ["status", "--short"], ["status", "--porcelain"],
                 ["status", "--untracked-files=no"], ["diff"], ["diff", "--cached"],
                 ["diff", "--stat", "--", "file"], ["add", "file"], ["add", "-u"],
                 ["add", "-A", "--", "file"], ["commit", "-m", "-S secret"],
                 ["commit", "-F", "message.txt"], ["commit", "--amend", "-m", "message"],
                 ["switch", "-c", "topic"], ["log", "--oneline", "-5"],
                 ["log", "-n", "2"], ["config", "--get", "user.name"],
                 ["rev-parse", "--show-toplevel"], ["rev-parse", "--is-inside-work-tree"],
                 ["symbolic-ref", "--short", "HEAD"], ["cat-file", "-p", "HEAD"]]
        for args in cases:
            with self.subTest(args=args):
                self.assertIsNotNone(API["grammar"](args))

    def test_unsupported_grammar(self):
        cases = [["--version"], ["help"], ["st"], ["rebase", "-i"],
                 ["status", "-z"], ["status", "--branch"], ["status", "--unknown"],
                 ["add", "-p"], ["add", "-f", "file"], ["add", "--force", "file"],
                 ["commit", "-S", "-m", "x"], ["commit"],
                 ["commit", "-m"], ["commit", "-m", "x", "-F", "file"],
                 ["commit", "-F", "-"], ["commit", "-F", "~message"], ["-C", "~repo", "status"],
                 ["switch", "main"], ["switch", "--detach"], ["switch", "a", "b"], ["log", "--format=%H"],
                 ["config", "--get-all", "user.name"], ["for-each-ref"],
                 ["rev-parse", "--git-dir"], ["symbolic-ref", "HEAD", "refs/heads/x"],
                 ["rev-parse", "HEAD"], ["rev-parse", "--verify", "HEAD"],
                 ["cat-file", "-p", "HEAD..HEAD"], ["switch", "foo//bar"], ["switch", "foo/"],
                 ["cat-file", "--batch"], ["diff", "file"], ["--bare", "status"],
                 ["--git-dir=.git", "status"], ["--work-tree=.", "status"],
                 ["-c", "alias.st=status", "status"], ["-c", "core.fsmonitor=true", "status"],
                 ["-C"], ["add", "--", ":(literal)file"], ["add", "--", "*.txt"]]
        for args in cases:
            with self.subTest(args=args):
                self.assertIsNone(API["grammar"](args))

    def test_exec_exact_once_and_stdin(self):
        self.fake()
        for args, backend in ((["status", "--short"], "ngit"),
                              (["-c", "user.name=Other", "-C", ".", "-C", "", "status", "--short"], "ngit"),
                              (["add", "-p", "--", "space file"], "git"),
                              ([*MAGIT, "diff", "--", "file"], "ngit"),
                              ([*MAGIT, "diff", "--", "*.txt"], "git")):
            with self.subTest(args=args):
                record = self.base / "record"
                record.unlink(missing_ok=True)
                self.env["TEST_EXIT"] = "37"
                result = self.dispatch(args, data=b"binary\x00input\n")
                self.assertEqual(result[0], 37)
                records = [json.loads(x) for x in record.read_text().splitlines()]
                self.assertEqual(len(records), 1)
                expected = [x for x in args if x != "--literal-pathspecs"] if backend == "ngit" else args
                self.assertEqual(records[0]["args"], expected)
                self.assertEqual(records[0]["stdin"], base64.b64encode(b"binary\x00input\n").decode())
                self.assertEqual(records[0]["pid"], result[3])
                self.assertEqual(self.backend(), backend)

    def test_symlink_resolves_sibling_ngit(self):
        self.fake()
        del self.env["NGIT_COMMAND"]
        installed = self.base / "application/bin"
        installed.mkdir(parents=True)
        shutil.copy2(DISPATCH, installed / "ngit-git")
        shutil.copy2(self.base / "ngit-fake", installed / "ngit")
        link = self.base / "launcher"
        link.symlink_to(installed / "ngit-git")
        self.assertEqual(run([str(link), "status"], self.repo, self.env)[0], 0)
        record = json.loads((self.base / "record").read_text())
        self.assertEqual(record["backend"], "ngit")

    def test_magit_metadata_and_writes(self):
        self.fake()
        self.env["INSIDE_EMACS"] = "30.2,magit"
        for args in (["rev-parse", "--show-toplevel"], ["config", "--get", "user.name"],
                     ["symbolic-ref", "HEAD"], ["cat-file", "-t", "HEAD"]):
            self.dispatch([*MAGIT, *args])
            item = json.loads((self.base / "trace").read_text().splitlines()[-1])
            self.assertEqual((item["backend"], item["reason"]), ("git", "magit-metadata"))
        self.dispatch([*MAGIT, "add", "--", "file"])
        self.assertEqual(self.backend(), "ngit")

    def test_exec_failure_never_replays(self):
        self.fake()
        self.env["NGIT_COMMAND"] = str(self.base / "missing")
        result = self.dispatch(["status"])
        self.assertEqual(result[0], 127)
        self.assertEqual(self.backend(), "ngit")
        self.assertFalse((self.base / "record").exists())

    def test_force_invalid_and_trace_secrets(self):
        self.fake()
        self.env["NGIT_GIT_BACKEND"] = "git"
        self.dispatch(["commit", "-m", "credential=SECRET"])
        self.assertEqual(self.backend(), "git")
        self.assertNotIn("SECRET", (self.base / "trace").read_text())
        self.env["NGIT_GIT_BACKEND"] = "invalid"
        before = (self.base / "record").read_bytes()
        self.assertEqual(self.dispatch(["status"])[0], 2)
        self.assertEqual((self.base / "record").read_bytes(), before)

    def test_classifier_negative_control(self):
        mutant = (DISPATCH.read_text().replace('"--porcelain=v1", "--long",',
                                               '"--porcelain=v1", "--long", "-z",'))
        scope = {"__name__": "mutant"}
        exec(compile(mutant, "mutant", "exec"), scope)
        self.assertIsNotNone(scope["grammar"](["status", "-z"]))
        with self.assertRaises(AssertionError):
            self.assertIsNone(scope["grammar"](["status", "-z"]))


@unittest.skipUnless(MODE in ("all", "boundaries"), "mode")
class Boundaries(Fixture):
    def fallback(self):
        self.dispatch(["status", "--short"])
        self.assertEqual(self.backend(), "git")

    def test_inherited_pager_known_values_or_explicit_no_pager(self):
        self.fake()
        self.env["GIT_PAGER"] = "arbitrary-pager"
        self.dispatch(["add", "file"])
        self.assertEqual(self.backend(), "git")
        self.dispatch(["-c", "user.name=-P", "status"])
        self.assertEqual(self.backend(), "git")
        self.dispatch([*MAGIT, "add", "--", "file"])
        self.assertEqual(self.backend(), "ngit")
        for pager in ("cat", ""):
            self.env["GIT_PAGER"] = pager
            self.dispatch(["add", "file"])
            self.assertEqual(self.backend(), "ngit")

    def test_switch_existing_target_attributes_uses_git(self):
        original = self.git("symbolic-ref", "--short", "HEAD").decode().strip()
        self.git("switch", "-q", "-c", "encoded")
        (self.repo / ".gitattributes").write_text("*.txt working-tree-encoding=UTF-16LE\n")
        (self.repo / "x.txt").write_bytes(b"x\0\n\0")
        self.git("add", ".gitattributes", "x.txt")
        self.git("commit", "-qm", "encoded target")
        self.git("switch", "-q", original)
        self.assertFalse((self.repo / ".gitattributes").exists())
        reference = self.base / "reference"
        shutil.copytree(self.repo, reference)
        args = ["switch", "encoded"]
        expected = run([GIT, *args], reference, self.env)[:3]
        self.assertEqual(self.dispatch(args)[:3], expected)
        self.assertEqual(self.backend(), "git")
        self.assertEqual((self.repo / "x.txt").read_bytes(), b"x\0\n\0")
        self.assertEqual((self.repo / "x.txt").read_bytes(), (reference / "x.txt").read_bytes())

    def test_attributes_sources_fallback_and_utf16_index(self):
        attr = self.repo / ".gitattributes"
        attr.write_text("*.txt working-tree-encoding=UTF-16LE\n")
        (self.repo / "encoded.txt").write_bytes(b"x\0\n\0")
        reference = self.base / "reference"
        shutil.copytree(self.repo, reference)
        args = ["add", "encoded.txt"]
        self.assertEqual(self.dispatch(args)[:3], run([GIT, *args], reference, self.env)[:3])
        self.assertEqual(self.backend(), "git")
        self.assertEqual(self.git("show", ":encoded.txt"), b"x\n")
        self.assertEqual(self.git("rev-parse", ":encoded.txt"),
                         run([GIT, "rev-parse", ":encoded.txt"], reference, self.env)[1])
        attr.unlink()
        (self.repo / ".gitignore").write_text("ignored/\n")
        global_attr = Path(self.git("var", "GIT_ATTR_GLOBAL").decode().removesuffix("\n"))
        for source in (self.repo / "ignored/deep/.gitattributes",
                       self.repo / ".git/info/attributes", global_attr):
            with self.subTest(source=source.name):
                source.parent.mkdir(parents=True, exist_ok=True)
                source.write_text("* ident\n")
                args = ["status", "--short"]
                self.assertEqual(self.dispatch(args)[:3], run([GIT, *args], self.repo, self.env)[:3])
                self.assertEqual(self.backend(), "git")
                source.unlink()
        global_attr.symlink_to(self.base / "missing-attributes")
        self.fallback()
        global_attr.unlink()
        external = self.base / "external"
        external.mkdir()
        (external / ".gitattributes").write_text("* ident\n")
        link = self.repo / "linked"
        link.symlink_to(external, target_is_directory=True)
        self.fallback()
        link.unlink()
        # A relative XDG path is interpreted after Git's -C; never guess it
        # relative to the dispatcher's original working directory.
        old_xdg = self.env["XDG_CONFIG_HOME"]
        relative_attr = self.repo / "subproject/default/git/attributes"
        relative_attr.parent.mkdir(parents=True)
        relative_attr.write_text("* ident\n")
        self.env["XDG_CONFIG_HOME"] = "default"
        args = ["-C", "subproject", "status", "--short"]
        self.assertEqual(self.dispatch(args)[:3], run([GIT, *args], self.repo, self.env)[:3])
        self.assertEqual(self.backend(), "git")
        self.env["XDG_CONFIG_HOME"] = old_xdg
        relative_attr.unlink()
        attr.write_text("* ident\n")
        self.git("add", ".gitattributes")
        attr.unlink()
        self.fallback()  # An index-only attributes file remains active.

    def test_revision_paths_and_ranges_use_exact_git(self):
        for args in (["rev-parse", "HEAD..HEAD"], ["rev-parse", "file"],
                     ["rev-parse", "HEAD"], ["rev-parse", "--verify", "HEAD"],
                     ["rev-parse", "--abbrev-ref", "HEAD"]):
            with self.subTest(args=args):
                self.assertEqual(self.dispatch(args)[:3], run([GIT, *args], self.repo, self.env)[:3])
                self.assertEqual(self.backend(), "git")

    def test_autocrlf_input_diff_warning_bytes(self):
        self.git("config", "core.autocrlf", "input")
        (self.repo / "file").write_bytes(b"one\r\ntwo\r\n")
        args = ["diff", "--", "file"]
        expected = run([GIT, *args], self.repo, self.env)[:3]
        self.assertIn(b"warning:", expected[2])
        self.assertEqual(self.dispatch(args)[:3], expected)
        self.assertEqual(self.backend(), "git")

    def test_i18n_log_encoding_uses_exact_git(self):
        self.git("commit", "--allow-empty", "-qm", "caf\u00e9")
        self.git("config", "i18n.logOutputEncoding", "ISO-8859-1")
        args = ["log", "--oneline", "-1"]
        expected = run([GIT, *args], self.repo, self.env)[:3]
        self.assertIn(b"caf\xe9", expected[1])
        self.assertEqual(self.dispatch(args)[:3], expected)
        self.assertEqual(self.backend(), "git")

    def test_clean_and_config_rejections(self):
        self.fake()
        self.dispatch(["status"])
        self.assertEqual(self.backend(), "ngit")
        for key, value in (("filter.lfs.clean", "cat"), ("commit.gpgsign", "true"),
                           ("tag.gpgsign", "true"), ("core.fsmonitor", "true"),
                           ("core.sparseCheckout", "true"), ("core.splitIndex", "true"),
                           ("extensions.worktreeConfig", "true")):
            with self.subTest(key=key):
                self.git("config", key, value)
                self.fallback()
                self.git("config", "--unset", key)

    def test_explicitly_disabled_policies(self):
        self.fake()
        for key in ("core.autocrlf", "core.fsmonitor", "core.splitindex",
                    "core.sparsecheckout", "core.sparsecheckoutcone"):
            with self.subTest(key=key):
                self.git("config", key, "false")
                self.dispatch(["status", "--short"])
                self.assertEqual(self.backend(), "ngit")

    def test_autocrlf_input_lf_add(self):
        self.fake()
        self.git("config", "core.autocrlf", "input")
        self.dispatch(["add", "--", "file"])
        self.assertEqual(self.backend(), "ngit")

    def test_autocrlf_input_untracked_crlf_all_add(self):
        self.fake()
        self.git("config", "core.autocrlf", "input")
        (self.repo / "fresh").write_bytes(b"new\r\n")
        for args in (["add", "fresh"], ["add", "-A"], ["add", "--all"]):
            self.dispatch(args)
            self.assertEqual(self.backend(), "git")

    def test_force_add_ignored_crlf_preserves_git_argv(self):
        self.fake()
        self.git("config", "core.autocrlf", "input")
        (self.repo / ".gitignore").write_text("ignored\n")
        (self.repo / "ignored").write_bytes(b"ignored\r\n")
        for args in (["add", "-f", "ignored"], ["add", "--force", "ignored"]):
            self.dispatch(args)
            self.assertEqual(self.backend(), "git")
            record = json.loads((self.base / "record").read_text().splitlines()[-1])
            self.assertEqual(record["args"], args)

    def test_status_default_config_uses_git_bytes(self):
        for key, args in (("status.short", ["status"]),
                          ("status.branch", ["status", "--short"])):
            self.git("config", key, "true")
            self.assertEqual(self.dispatch(args)[:3], run([GIT, *args], self.repo, self.env)[:3])
            self.assertEqual(self.backend(), "git")
            self.git("config", "--unset", key)

    def test_replacement_log_loose_and_packed(self):
        tree = self.git("rev-parse", "HEAD^{tree}").decode().strip()
        replacement = self.git("commit-tree", tree, "-m", "replacement").decode().strip()
        self.git("replace", "HEAD", replacement)
        for packed in (False, True):
            if packed:
                self.git("pack-refs", "--all")
            args = ["log", "--oneline", "-3"]
            expected = run([GIT, *args], self.repo, self.env)[:3]
            self.assertIn(b"replacement", expected[1])
            self.assertEqual(self.dispatch(args)[:3], expected)
            self.assertEqual(self.backend(), "git")

    def test_encoded_commit_fallback_matches_git_sha(self):
        self.git("config", "i18n.commitEncoding", "ISO-8859-1")
        reference = self.base / "reference"
        shutil.copytree(self.repo, reference)
        args = ["commit", "--allow-empty", "-q", "-m", "caf\u00e9"]
        expected = run([GIT, *args], reference, self.env)[:3]
        self.assertEqual(self.dispatch(args)[:3], expected)
        self.assertEqual(self.backend(), "git")
        self.assertEqual(self.git("rev-parse", "HEAD"),
                         run([GIT, "rev-parse", "HEAD"], reference, self.env)[1])
        self.assertIn(b"encoding ISO-8859-1\n", self.git("cat-file", "-p", "HEAD"))

    def test_special_index_flags(self):
        for option, undo in (("--assume-unchanged", "--no-assume-unchanged"),
                             ("--skip-worktree", "--no-skip-worktree")):
            self.git("update-index", option, "file")
            self.fallback()
            self.git("update-index", undo, "file")
        (self.repo / "intent").write_text("new\n")
        self.git("add", "-N", "intent")
        self.fallback()

    def test_env_and_operation_boundaries(self):
        for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_LITERAL_PATHSPECS",
                    "GIT_CONFIG_COUNT", "GIT_OBJECT_DIRECTORY"):
            with self.subTest(key=key):
                self.env[key] = "1"
                self.fallback()
                del self.env[key]
        (self.repo / ".git/MERGE_HEAD").touch()
        self.fallback()

    def test_include_is_effective(self):
        included = self.base / "included"
        included.write_text('[filter "hidden"]\n clean = cat\n')
        self.git("config", "include.path", str(included))
        self.fallback()

    def test_certificate_probe_error_and_timeout(self):
        with mock.patch.object(subprocess, "run", side_effect=OSError("unavailable")):
            self.assertFalse(API["certificate"](GIT, []))
        with mock.patch.object(subprocess, "run", side_effect=subprocess.TimeoutExpired("git", 5)):
            self.assertFalse(API["certificate"](GIT, []))

    def test_sha256_bare_invalid_and_indirection(self):
        original = self.repo
        for name, args in (("sha256", ["--object-format=sha256"]), ("bare", ["--bare"])):
            self.repo = self.base / name
            self.repo.mkdir()
            self.git("init", "-q", *args)
            self.fallback()
        self.repo = self.base
        self.fallback()
        self.repo = original
        self.git("worktree", "add", "-q", str(self.base / "linked"), "-b", "linked")
        self.repo = self.base / "linked"
        self.fallback()

    def test_submodule_hook_and_split_index(self):
        (self.repo / ".gitmodules").touch()
        self.fallback()
        (self.repo / ".gitmodules").unlink()
        hook = self.repo / ".git/hooks/pre-commit"
        hook.write_text("#!/bin/sh\nexit 0\n")
        hook.chmod(0o755)
        self.fallback()
        hook.unlink()
        self.git("update-index", "--split-index")
        self.fallback()

    def test_gitlink_without_gitmodules(self):
        head = self.git("rev-parse", "HEAD").decode().strip()
        self.git("update-index", "--add", "--cacheinfo", "160000", head, "submodule")
        self.fallback()


@unittest.skipUnless(MODE in ("all", "e2e"), "mode")
class EndToEnd(Fixture):
    def test_normal_inherited_cat_add_commit(self):
        self.env.pop("INSIDE_EMACS", None)
        self.env.update(GIT_PAGER="cat",
                        NGIT_NELISP=os.environ.get("NGIT_NELISP") or shutil.which("nelisp"))
        (self.repo / "file").write_bytes(b"one\ntwo\n")
        reference = self.base / "reference"
        shutil.copytree(self.repo, reference)
        for args in (["add", "--", "file"], ["commit", "-q", "-m", "CLI inherited cat"]):
            expected = run([GIT, *args], reference, self.env)[:3]
            self.assertEqual(self.dispatch(args)[:3], expected)
            self.assertEqual(self.backend(), "ngit")
        self.assertEqual(self.git("rev-parse", "HEAD"),
                         run([GIT, "rev-parse", "HEAD"], reference, self.env)[1])
        self.assertEqual(self.git("show", "HEAD:file"), b"one\ntwo\n")

    def test_magit_no_pager_inherited_pager_add_commit(self):
        self.env.update(GIT_PAGER="cat", INSIDE_EMACS="30.2,magit",
                        NGIT_NELISP=os.environ.get("NGIT_NELISP") or shutil.which("nelisp"))
        (self.repo / "file").write_bytes(b"one\ntwo\n")
        reference = self.base / "reference"
        shutil.copytree(self.repo, reference)
        for command in (["add", "--", "file"], ["commit", "-q", "-m", "Magit inherited pager"]):
            args = [*MAGIT, *command]
            expected = run([GIT, *args], reference, self.env)[:3]
            self.assertEqual(self.dispatch(args)[:3], expected)
            self.assertEqual(self.backend(), "ngit")
        self.assertEqual(self.git("rev-parse", "HEAD"),
                         run([GIT, "rev-parse", "HEAD"], reference, self.env)[1])
        self.assertEqual(self.git("show", "HEAD:file"), b"one\ntwo\n")

    def test_switch_create_current_certified_tree(self):
        self.env["NGIT_NELISP"] = os.environ.get("NGIT_NELISP") or shutil.which("nelisp")
        reference = self.base / "reference"
        shutil.copytree(self.repo, reference)
        args = ["switch", "-c", "topic"]
        expected = run([GIT, *args], reference, self.env)[:3]
        self.assertEqual(self.dispatch(args)[:3], expected)
        self.assertEqual(self.backend(), "ngit")
        self.assertEqual(self.git("branch", "--show-current"), b"topic\n")
        self.assertEqual((self.repo / "file").read_bytes(), b"one\n")

    def test_autocrlf_input_crlf_parity(self):
        self.env["NGIT_NELISP"] = os.environ.get("NGIT_NELISP") or shutil.which("nelisp")
        self.git("config", "core.autocrlf", "input")
        (self.repo / "file").write_bytes(b"one\r\ntwo\r\n")
        reference = self.base / "reference"
        shutil.copytree(self.repo, reference)
        for args in (["status", "--porcelain"], ["add", "--", "file"],
                     ["commit", "-q", "-m", "CRLF input"]):
            with self.subTest(args=args):
                expected = run([GIT, *args], reference, self.env)[:3]
                actual = self.dispatch(args)[:3]
                self.assertEqual(self.backend(), "git" if args[0] == "add" else "ngit")
                self.assertEqual(actual, expected)
        self.assertEqual(self.git("rev-parse", "HEAD"),
                         run([GIT, "rev-parse", "HEAD"], reference, self.env)[1])
        for repo in (self.repo, reference):
            (repo / "fresh").write_bytes(b"new\r\n")
        for args in (["add", "-A"], ["commit", "-q", "-m", "untracked CRLF"]):
            expected = run([GIT, *args], reference, self.env)[:3]
            self.assertEqual(self.dispatch(args)[:3], expected)
            self.assertEqual(self.backend(), "git" if args[0] == "add" else "ngit")
        self.assertEqual(self.git("rev-parse", "HEAD"),
                         run([GIT, "rev-parse", "HEAD"], reference, self.env)[1])

    def test_real_read_and_write_parity(self):
        runtime = os.environ.get("NGIT_NELISP") or shutil.which("nelisp")
        self.assertTrue(runtime, "NGIT_NELISP or nelisp executable required")
        self.env["NGIT_NELISP"] = runtime
        (self.repo / "file").write_text("one\ntwo\n")
        commands = [["status", "--porcelain"], ["diff"], ["diff", "--stat"],
                    [*MAGIT, "diff", "--", "file"], ["log", "--oneline", "-1"],
                    ["config", "--get", "user.name"], ["rev-parse", "--is-inside-work-tree"],
                    ["symbolic-ref", "--short", "HEAD"], ["cat-file", "-t", "HEAD"]]
        for args in commands:
            with self.subTest(args=args):
                expected = run([GIT, *args], self.repo, self.env)[:3]
                actual = self.dispatch(args)[:3]
                self.assertEqual(self.backend(), "ngit")
                self.assertEqual(actual, expected)
        self.assertEqual(self.dispatch(["add", "--", "file"])[0], 0)
        self.assertEqual(self.backend(), "ngit")
        self.assertEqual(self.git("diff", "--cached", "--name-only"), b"file\n")
        self.assertEqual(self.dispatch(["commit", "-q", "-m", "second"])[0], 0)
        self.assertEqual(self.backend(), "ngit")
        self.assertEqual(self.git("log", "-1", "--format=%s"), b"second\n")
        self.assertEqual(self.dispatch(["switch", "-c", "topic"])[0], 0)
        self.assertEqual(self.backend(), "ngit")
        self.assertEqual(self.git("branch", "--show-current"), b"topic\n")
        (self.repo / "*.txt").write_text("literal\n")
        args = [*MAGIT, "add", "--", "*.txt"]
        self.assertEqual(self.dispatch(args)[0], 0)
        self.assertEqual(self.backend(), "git")
        self.assertEqual(self.git("diff", "--cached", "--name-only"), b"*.txt\n")


if __name__ == "__main__":
    if MODE not in ("all", "routing", "boundaries", "e2e"):
        raise SystemExit("expected routing, boundaries, e2e, or all")
    unittest.main(verbosity=2)
