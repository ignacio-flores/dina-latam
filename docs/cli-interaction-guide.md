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

There is no separate "main menu" layer. Plain `dina` shows the dashboard and its
immediate `DINA Actions`; browsing commands opens the navigator directly.
