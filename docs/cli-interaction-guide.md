# DINA CLI Interaction Guide

This guide is the source of truth for interactive behavior and terminal styling
in the DINA CLI. User workflow docs can summarize these rules, but new CLI code
should follow this contract directly.

## Menus

- Use `dina_menu_select()` for action choices.
- Use `dina_menu_confirm()` for yes/no confirmations in new CLI paths.
- Do not add raw `[y/N]` prompts; confirmations should use the shared
  confirmation menu.
- Do not add new letter-shortcut menus such as `[a/b/q]`.
- Use text entry helpers only for free-form values such as task selectors,
  paths, config keys, or replacement parameter values.
- Keep inspection commands readable and script-friendly. When a command needs a
  choice, show a separate action menu instead of making tables interactive.

## Menu Controls

- In an interactive terminal, menus support arrow navigation: Up/Down moves,
  Left/Right moves backward or forward when available, and Enter selects.
- Numbered choices are always displayed and remain the fallback interaction:
  type the number and press Enter.
- `q` quits when quitting is allowed.
- `?` shows choice-specific help when available.
- In the numbered fallback, `p` and `n` move backward or forward when a menu
  includes those actions.
- Blank Enter should only run an action when that menu explicitly sets that
  action as the default. Use `default = "quit"` when blank Enter must mean no
  action.

## Text And Color

- Use `dina_cli_header()` for page headers.
- Use `dina_cli_dim()` for secondary prose, metadata, and hints.
- Use `dina_cli_command()` for commands or command templates.
- Use `dina_cli_alert()`, `dina_cli_ok()`, `dina_cli_warn()`, and
  `dina_cli_err()` for status messages.
- Do not put raw ANSI color sequences in feature code. Keep color decisions
  centralized in the CLI wrapper helpers.

## Compatibility

`dina commands` and `dina navigate` are the documented command navigator
entrypoints. `dina menu` and `dina menu commands` remain compatibility aliases
for the navigator, but should not introduce a separate menu layer.

Every user-facing command listed in `dina commands` must appear in `dina help`
or the relevant topic help. Every documented topic-help command must appear in
the command navigator unless it is intentionally hidden and covered by a test.

Plain `dina` opens Configuration, Sources, Pipeline, and Results with concise
statuses. One secondary suggestion can accompany them. `dina commands` contains
update lifecycle actions, project details, setup, and maintenance. Inspection
returns to its originating area; it never launches a runner or viewer implicitly.
The noninteractive home prints the same statuses and equivalent commands.

## Source family review

- Menus and typed commands share the `Explore -> Include` workflow.
- Source families must be selected explicitly; generic actions never silently
  substitute SNA. Keep the family selected until the user returns to families.
- Explore presents coverage, overlapping revisions, then problems. Detailed
  evidence is saved with the candidate, not resolved through a mutable latest
  inventory. Explain unavailable comparisons and their cause. Do not label empty
  cells as skipped checks: distinguish expected absence, missing expected values,
  outside extracted coverage, and undetermined applicability.
- Family selection, home, and recommendations share one status calculation.
  Included sources do not imply rebuilt outputs. No incoming files says nothing
  about updates available from the producer.
- Reports begin with a country summary, then coverage, value changes, and problems.
  Print revisions ahead of absence evidence; group blockers before warnings.
  Long records use labeled fields rather than truncated columns. Full source IDs
  remain available. Expected absences are not problems.
- Include accepts the exact saved family review after one confirmation. Scripts
  use `--confirm`. Do not add another generic navigator confirmation around it.
- Keep explicit legacy assessment flags for compatibility, but do not surface
  dry-run terminology, staging directories or required run paths in normal actions.

## Configuration, pipeline, and results

- Configuration uses the active update's commented YAML file, not a settings wizard.
  The menu compares benchmark and update values above its actions, emphasizing
  changed values with color and `*`. Baseline filenames and warnings appear together.
  Full paths and setting keys are under **Details and file paths**. Values in the
  update file override benchmark defaults.
  `dina update config show` is compact; `--full` exposes effective YAML and waits
  for Enter when opened from the menu. `q` returns to the workspace.
  `edit` explains the editor's save/exit keys and waits for the editor to return;
  `check` runs the same validation used after editing. A missing baseline affects export,
  not source inspection. Resume preserves comments and manual changes.
- Home shows recorded pipeline outcomes immediately. File freshness is inspected
  when you open Pipeline or use `dina run list`, so startup does not scan all tasks.
  Pipeline shows the last recorded execution and declared output observations
  separately. `dina run list` and `dina run why TASK` are the inspection interface;
  existing execution commands and flags retain their behavior.
- `dina results show` lists existing final WID graphs with generation context.
  Only explicit selection or `dina results open GRAPH` opens a viewer. Historical
  graphs without metadata say "Generation baseline unknown". A changed baseline,
  settings, or generated artifact requires regeneration through the export step.
