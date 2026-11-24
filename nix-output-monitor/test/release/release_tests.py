#!/usr/bin/env python3
"""Check the release workflow invariants of .github/workflows/release.yml."""

import os
import pathlib
import subprocess
import sys
import tempfile

import yaml

REPO = pathlib.Path(os.environ.get("NCD_REPO", ".")).resolve()
WORKFLOW = pathlib.Path(
    os.environ.get("NCD_WORKFLOW", REPO / ".github/workflows/release.yml")
)
PLAYBOOK = pathlib.Path(
    os.environ.get(
        "NCD_PLAYBOOK",
        REPO / "nix-output-monitor/test/release/rerun-playbook.md",
    )
)

SHELL = os.environ.get("NCD_SHELL", "/bin/sh")

GIT_STUB = f"""#!{SHELL}
printf '%s\\n' "$*" >> "$STUB_GIT_LOG"
case "$1" in
  ls-remote)
    if [ -n "$STUB_REMOTE_SHA" ]; then
      printf '%s\\trefs/tags/%s\\n' "$STUB_REMOTE_SHA" "$TAG"
    fi
    ;;
  rev-parse)
    printf '%s\\n' "$STUB_EXISTING_SHA"
    ;;
esac
exit 0
"""


def workflow():
    return yaml.safe_load(WORKFLOW.read_text())


def on_block(document):
    return document[True]


def prepare_run(document):
    return document["jobs"]["prepare"]["steps"][0]["run"]


def tag_run(document):
    return document["jobs"]["release"]["steps"][1]["run"]


def publish_step(document):
    return document["jobs"]["release"]["steps"][2]


def parse_output(text):
    values = {}
    for line in text.splitlines():
        if not line:
            continue
        key, _, value = line.partition("=")
        values[key] = value
    return values


def run_prepare(document, directory, ref_type, ref_name, input_tag, commit_message):
    script = directory / "prepare.sh"
    script.write_text(prepare_run(document))
    output = directory / "prepare.out"
    output.write_text("")
    environment = os.environ.copy()
    environment.update(
        GITHUB_OUTPUT=str(output),
        REF_TYPE=ref_type,
        REF_NAME=ref_name,
        INPUT_TAG=input_tag,
        COMMIT_MESSAGE=commit_message,
    )
    result = subprocess.run(
        [SHELL, str(script)],
        env=environment,
        capture_output=True,
        text=True,
        check=False,
    )
    return result, parse_output(output.read_text())


def run_tag_step(document, directory, github_sha, tag, remote_sha, existing_sha):
    script = directory / "tagstep.sh"
    script.write_text(tag_run(document))
    binary_directory = directory / "bin"
    binary_directory.mkdir(exist_ok=True)
    git = binary_directory / "git"
    git.write_text(GIT_STUB)
    git.chmod(0o755)
    log = directory / "git.log"
    log.write_text("")
    environment = os.environ.copy()
    environment.update(
        PATH=f"{binary_directory}:{environment['PATH']}",
        GITHUB_SHA=github_sha,
        TAG=tag,
        STUB_REMOTE_SHA=remote_sha,
        STUB_EXISTING_SHA=existing_sha,
        STUB_GIT_LOG=str(log),
    )
    result = subprocess.run(
        [SHELL, str(script)],
        env=environment,
        capture_output=True,
        text=True,
        check=False,
    )
    return result, log.read_text().splitlines()


def assert_prepare_tag(ref_type, ref_name, input_tag, commit_message, expected):
    document = workflow()
    with tempfile.TemporaryDirectory() as directory_name:
        result, outputs = run_prepare(
            document,
            pathlib.Path(directory_name),
            ref_type,
            ref_name,
            input_tag,
            commit_message,
        )
    assert (
        result.returncode == 0
    ), f"prepare exited {result.returncode}: {result.stderr}"
    assert outputs.get("tag") == expected, f"expected tag={expected!r}, got {outputs!r}"


# INVENTORY: release.1.1
def test_push_to_main_starts_the_workflow():
    document = workflow()
    branches = on_block(document)["push"]["branches"]
    assert "main" in branches, f"on.push.branches is {branches!r}, main is missing"


# INVENTORY: release.1.2
def test_version_tag_push_starts_the_workflow():
    document = workflow()
    tags = on_block(document)["push"]["tags"]
    assert "v*" in tags, f"on.push.tags is {tags!r}, v* is missing"


# INVENTORY: release.1.3
def test_manual_dispatch_requires_the_tag_input():
    document = workflow()
    required = on_block(document)["workflow_dispatch"]["inputs"]["tag"]["required"]
    assert required is True, f"workflow_dispatch.inputs.tag.required is {required!r}"


# INVENTORY: release.1.4
def test_other_branches_do_not_start_the_workflow():
    document = workflow()
    branches = on_block(document)["push"]["branches"]
    assert branches == [
        "main"
    ], f"on.push.branches is {branches!r}, not exactly ['main']"


# INVENTORY: release.1.5
def test_other_tags_do_not_start_the_workflow():
    document = workflow()
    tags = on_block(document)["push"]["tags"]
    assert tags == ["v*"], f"on.push.tags is {tags!r}, not exactly ['v*']"


# INVENTORY: release.1.6
def test_pull_requests_do_not_start_the_release_workflow():
    document = workflow()
    triggers = sorted(str(key) for key in on_block(document))
    assert triggers == ["push", "workflow_dispatch"], f"triggers are {triggers!r}"


# INVENTORY: release.2.1
def test_runs_share_the_release_concurrency_group():
    document = workflow()
    group = document["concurrency"]["group"]
    assert group == "release", f"concurrency.group is {group!r}"


# INVENTORY: release.2.2
def test_newer_runs_do_not_cancel_a_release():
    document = workflow()
    cancel = document["concurrency"].get("cancel-in-progress")
    assert cancel is False, f"concurrency.cancel-in-progress is {cancel!r}"


# INVENTORY: release.3.1
def test_the_workflow_grants_contents_write():
    document = workflow()
    contents = document["permissions"]["contents"]
    assert contents == "write", f"permissions.contents is {contents!r}"


# INVENTORY: release.3.2
def test_only_contents_write_is_granted():
    document = workflow()
    keys = list(document["permissions"])
    assert keys == ["contents"], f"permissions keys are {keys!r}"


# INVENTORY: release.4.1
def test_a_tag_ref_is_the_release_tag():
    assert_prepare_tag("tag", "v1.2.3", "", "", "v1.2.3")


# INVENTORY: release.4.2
def test_a_manual_dispatch_uses_the_tag_input():
    assert_prepare_tag("branch", "main", "v9.9.9", "", "v9.9.9")


# INVENTORY: release.4.3
def test_a_tag_ref_wins_over_the_manual_input():
    assert_prepare_tag("tag", "v1", "v9", "", "v1")


# INVENTORY: release.4.4
def test_a_version_commit_message_yields_that_tag():
    assert_prepare_tag("branch", "main", "", "v1.2.3 release notes", "v1.2.3")


# INVENTORY: release.4.5
def test_only_the_first_commit_message_line_is_considered():
    assert_prepare_tag("branch", "main", "", "release notes\nv1.0.0", "")


# INVENTORY: release.4.6
def test_a_non_version_commit_message_yields_an_empty_tag():
    assert_prepare_tag("branch", "main", "", "release 1.0", "")


# INVENTORY: release.4.7
def test_the_input_tag_is_passed_through_unvalidated():
    assert_prepare_tag("branch", "main", "not a tag!", "", "not a tag!")


# INVENTORY: release.4.8
def test_the_resolved_tag_is_the_prepare_output():
    document = workflow()
    with tempfile.TemporaryDirectory() as directory_name:
        _, outputs = run_prepare(
            document, pathlib.Path(directory_name), "tag", "v1.2.3", "", ""
        )
    assert outputs.get("tag") == "v1.2.3", f"GITHUB_OUTPUT is {outputs!r}"
    output = document["jobs"]["prepare"]["outputs"]["tag"]
    assert (
        output == "${{ steps.tag.outputs.tag }}"
    ), f"prepare.outputs.tag is {output!r}"


# INVENTORY: release.4.9
def test_leading_whitespace_yields_an_empty_tag():
    assert_prepare_tag("branch", "main", "", " v1.2.3", "")


# INVENTORY: release.5.1
def test_an_empty_tag_skips_the_release_job():
    document = workflow()
    condition = document["jobs"]["release"]["if"]
    assert (
        condition == "needs.prepare.outputs.tag != ''"
    ), f"release.if is {condition!r}"


# INVENTORY: release.5.2
def test_the_release_job_is_limited_to_ten_minutes():
    document = workflow()
    timeout = document["jobs"]["release"]["timeout-minutes"]
    assert timeout == 10, f"release.timeout-minutes is {timeout!r}"


# INVENTORY: release.5.3
def test_the_checkout_fetches_full_history():
    document = workflow()
    checkouts = [
        step
        for step in document["jobs"]["release"]["steps"]
        if "actions/checkout" in str(step.get("uses", ""))
    ]
    assert checkouts, "the release job has no actions/checkout step"
    depth = checkouts[0].get("with", {}).get("fetch-depth")
    assert depth == 0, f"checkout fetch-depth is {depth!r}"


# INVENTORY: release.5.4
def test_the_release_job_waits_for_prepare():
    document = workflow()
    needs = document["jobs"]["release"]["needs"]
    assert needs == "prepare", f"release.needs is {needs!r}"


# INVENTORY: release.6.1
def test_an_existing_tag_at_the_same_commit_proceeds():
    document = workflow()
    with tempfile.TemporaryDirectory() as directory_name:
        result, log = run_tag_step(
            document, pathlib.Path(directory_name), "abc", "v1", "abc", "abc"
        )
    assert (
        result.returncode == 0
    ), f"tag step exited {result.returncode}: {result.stderr}"
    assert any(line.startswith("fetch ") for line in log), f"git.log is {log!r}"
    assert not any(line.startswith("tag ") for line in log), f"git.log is {log!r}"
    assert not any(line.startswith("push ") for line in log), f"git.log is {log!r}"


# INVENTORY: release.6.2
def test_an_existing_tag_at_another_commit_fails():
    document = workflow()
    with tempfile.TemporaryDirectory() as directory_name:
        result, _ = run_tag_step(
            document, pathlib.Path(directory_name), "abc", "v1", "bbb", "bbb"
        )
    assert result.returncode == 1, f"tag step exited {result.returncode}, not 1"


# INVENTORY: release.6.3
def test_the_mismatch_message_names_the_tag_and_both_commits():
    document = workflow()
    with tempfile.TemporaryDirectory() as directory_name:
        result, _ = run_tag_step(
            document, pathlib.Path(directory_name), "abc", "v1", "bbb", "bbb"
        )
    expected = "Tag v1 already points to bbb, not abc"
    assert result.returncode == 1, f"tag step exited {result.returncode}, not 1"
    assert expected in result.stderr, f"stderr is {result.stderr!r}"


# INVENTORY: release.6.4
def test_a_missing_tag_is_created_and_pushed():
    document = workflow()
    with tempfile.TemporaryDirectory() as directory_name:
        result, log = run_tag_step(
            document, pathlib.Path(directory_name), "abc", "v1", "", ""
        )
    assert (
        result.returncode == 0
    ), f"tag step exited {result.returncode}: {result.stderr}"
    tag_lines = [
        index for index, line in enumerate(log) if line.startswith("tag v1 abc")
    ]
    push_lines = [
        index
        for index, line in enumerate(log)
        if line.startswith("push origin refs/tags/v1")
    ]
    assert tag_lines, f"git.log is {log!r}"
    assert push_lines, f"git.log is {log!r}"
    assert tag_lines[0] < push_lines[0], f"git.log is {log!r}"


# INVENTORY: release.6.5
def test_an_annotated_tag_is_compared_by_its_commit():
    document = workflow()
    with tempfile.TemporaryDirectory() as directory_name:
        result, log = run_tag_step(
            document, pathlib.Path(directory_name), "abc", "v1", "ttt", "abc"
        )
    assert (
        result.returncode == 0
    ), f"tag step exited {result.returncode}: {result.stderr}"
    assert not any(line.startswith("tag ") for line in log), f"git.log is {log!r}"
    assert not any(line.startswith("push ") for line in log), f"git.log is {log!r}"


# INVENTORY: release.7.1
def test_the_release_uses_the_resolved_tag_as_tag_name():
    document = workflow()
    tag_name = publish_step(document)["with"]["tag_name"]
    assert tag_name == "${{ needs.prepare.outputs.tag }}", f"tag_name is {tag_name!r}"


# INVENTORY: release.7.2
def test_the_release_title_is_the_resolved_tag():
    document = workflow()
    name = publish_step(document)["with"]["name"]
    assert name == "${{ needs.prepare.outputs.tag }}", f"name is {name!r}"


# INVENTORY: release.7.3
def test_generated_release_notes_are_enabled():
    document = workflow()
    enabled = publish_step(document)["with"]["generate_release_notes"]
    assert enabled is True, f"generate_release_notes is {enabled!r}"


# INVENTORY: release.7.4
def test_publication_is_skipped_when_tag_creation_fails():
    document = workflow()
    step = publish_step(document)
    assert "if" not in step, f"publish step has if={step.get('if')!r}"
    assert "continue-on-error" not in step, "publish step sets continue-on-error"


# INVENTORY: release.7.5
def test_a_rerun_updates_the_existing_release():
    document = workflow()
    uses = str(publish_step(document).get("uses", ""))
    assert uses.startswith(
        "softprops/action-gh-release@v2"
    ), f"publish uses is {uses!r}"
    with tempfile.TemporaryDirectory() as directory_name:
        result, log = run_tag_step(
            document,
            pathlib.Path(directory_name),
            "abc",
            "v9.9.9",
            "abc",
            "abc",
        )
    assert (
        result.returncode == 0
    ), f"tag step exited {result.returncode}: {result.stderr}"
    assert not any(line.startswith("push ") for line in log), f"git.log is {log!r}"
    text = PLAYBOOK.read_text()
    assert "v9.9.9" in text, "the playbook does not name the rerun tag"
    assert "422" in text, "the playbook does not name the failure signal"


def main():
    tests = [
        (
            "release.1.1 a push to main starts the workflow",
            test_push_to_main_starts_the_workflow,
        ),
        (
            "release.1.2 a version tag push starts the workflow",
            test_version_tag_push_starts_the_workflow,
        ),
        (
            "release.1.3 a manual dispatch requires the tag input",
            test_manual_dispatch_requires_the_tag_input,
        ),
        (
            "release.1.4 other branches do not start the workflow",
            test_other_branches_do_not_start_the_workflow,
        ),
        (
            "release.1.5 other tags do not start the workflow",
            test_other_tags_do_not_start_the_workflow,
        ),
        (
            "release.1.6 pull requests do not start the workflow",
            test_pull_requests_do_not_start_the_release_workflow,
        ),
        (
            "release.2.1 runs share the release group",
            test_runs_share_the_release_concurrency_group,
        ),
        (
            "release.2.2 newer runs do not cancel a release",
            test_newer_runs_do_not_cancel_a_release,
        ),
        (
            "release.3.1 the workflow grants contents write",
            test_the_workflow_grants_contents_write,
        ),
        (
            "release.3.2 only contents write is granted",
            test_only_contents_write_is_granted,
        ),
        ("release.4.1 a tag ref is the release tag", test_a_tag_ref_is_the_release_tag),
        (
            "release.4.2 a manual dispatch uses the tag input",
            test_a_manual_dispatch_uses_the_tag_input,
        ),
        (
            "release.4.3 a tag ref wins over the manual input",
            test_a_tag_ref_wins_over_the_manual_input,
        ),
        (
            "release.4.4 a version commit message yields that tag",
            test_a_version_commit_message_yields_that_tag,
        ),
        (
            "release.4.5 only the first commit message line is considered",
            test_only_the_first_commit_message_line_is_considered,
        ),
        (
            "release.4.6 a non-version commit message yields an empty tag",
            test_a_non_version_commit_message_yields_an_empty_tag,
        ),
        (
            "release.4.7 the input tag is passed through unvalidated",
            test_the_input_tag_is_passed_through_unvalidated,
        ),
        (
            "release.4.8 the resolved tag is the prepare output",
            test_the_resolved_tag_is_the_prepare_output,
        ),
        (
            "release.4.9 leading whitespace yields an empty tag",
            test_leading_whitespace_yields_an_empty_tag,
        ),
        (
            "release.5.1 an empty tag skips the release job",
            test_an_empty_tag_skips_the_release_job,
        ),
        (
            "release.5.2 the release job is limited to ten minutes",
            test_the_release_job_is_limited_to_ten_minutes,
        ),
        (
            "release.5.3 the checkout fetches full history",
            test_the_checkout_fetches_full_history,
        ),
        (
            "release.5.4 the release job waits for prepare",
            test_the_release_job_waits_for_prepare,
        ),
        (
            "release.6.1 an existing tag at the same commit proceeds",
            test_an_existing_tag_at_the_same_commit_proceeds,
        ),
        (
            "release.6.2 an existing tag at another commit fails",
            test_an_existing_tag_at_another_commit_fails,
        ),
        (
            "release.6.3 the mismatch message names the tag and both commits",
            test_the_mismatch_message_names_the_tag_and_both_commits,
        ),
        (
            "release.6.4 a missing tag is created and pushed",
            test_a_missing_tag_is_created_and_pushed,
        ),
        (
            "release.6.5 an annotated tag is compared by its commit",
            test_an_annotated_tag_is_compared_by_its_commit,
        ),
        (
            "release.7.1 the release uses the resolved tag as tag name",
            test_the_release_uses_the_resolved_tag_as_tag_name,
        ),
        (
            "release.7.2 the release title is the resolved tag",
            test_the_release_title_is_the_resolved_tag,
        ),
        (
            "release.7.3 generated release notes are enabled",
            test_generated_release_notes_are_enabled,
        ),
        (
            "release.7.4 publication is skipped when tag creation fails",
            test_publication_is_skipped_when_tag_creation_fails,
        ),
        (
            "release.7.5 a rerun updates the existing release",
            test_a_rerun_updates_the_existing_release,
        ),
    ]
    failures = 0
    for name, test in tests:
        try:
            test()
            print(f"ok {name}")
        except Exception as error:
            failures += 1
            print(f"FAIL {name}: {error}", file=sys.stderr)
    sys.exit(1 if failures else 0)


if __name__ == "__main__":
    main()
