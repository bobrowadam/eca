# Features

## Chat

Chat is the main feature of ECA, allowing user to talk with LLM to behave like an agent, making changes using tools or just planning changes and next steps.

### Behaviors

![](./images/features/chat-behaviors.png)

Behavior affect the prompt passed to LLM and the tools to include, the current supported behaviors are:

- `plan`: Useful to plan changes and define better LLM plan before changing code via agent mode.
- `agent`: Make changes to code via file changing tools.

### Tools

![](./images/features/tools.png)

ECA leverage tools to give more power to the LLM, this is the best way to make LLMs have more context about your codebase and behave like an agent.
It supports both MCP server tools + ECA native tools.

!!! info "Approval / permissions"

    By default, ECA ask to approve any tool, you can easily configure that, check `toolCall approval` [config](./config.md) or try the `plan` behavior.

### Native tools

ECA support built-in tools to avoid user extra installation and configuration, these tools are always included on models requests that support tools and can be [disabled/configured via config](./configuration.md) `nativeTools`.

#### Filesystem

Provides access to filesystem under workspace root, listing, reading and writing files, important for agentic operations.

- `eca_directory_tree`: list a directory as a tree (can be recursive).
- `eca_read_file`: read a file content.
- `eca_write_file`: write content to a new file.
- `eca_edit_file`: replace lines of a file with a new content.
- `eca_plan_edit_file`: Only used in plan mode, replace lines of a file with a new content.
- `eca_move_file`: move/rename a file.
- `eca_grep`: ripgrep/grep for paths with specified content.

#### Shell

Provides access to run shell commands, useful to run build tools, tests, and other common commands, supports exclude/include commands. 

- `eca_shell_command`: run shell command. Supports configs to exclude commands via `:nativeTools :shell :excludeCommands`.

#### Editor

Provides access to get information from editor workspaces.

- `eca_editor_diagnostics`: Ask client about the diagnostics (like LSP diagnostics).

#### Custom Tools

Besides the built-in native tools, ECA allows you to define your own tools by wrapping any command-line executable. This feature enables you to extend ECA's capabilities to match your specific workflows, such as running custom scripts, interacting with internal services, or using your favorite CLI tools.

Custom tools are configured in your `config.json` file. For a detailed guide on how to set them up, check the [Custom Tools configuration documentation](./configuration.md#custom-tools).

### Contexts

![](./images/features/contexts.png)

User can include contexts to the chat (`@`), including MCP resources, which can help LLM generate output with better quality.
Here are the current supported contexts types:

- `file`: a file in the workspace, server will pass its content to LLM (Supports optional line range).
- `directory`: a directory in the workspace, server will read all file contexts and pass to LLM.
- `repoMap`: a summary view of workspaces files and folders, server will calculate this and pass to LLM. Currently, the repo-map includes only the file paths in git.
- `mcpResource`: resources provided by running MCPs servers.

#### AGENTS.md automatic context

ECA will always include if found the `AGENTS.md` file as context, searching for both `/project-root/AGENTS.md` and `~/.config/eca/AGENTS.md`.

You can ask ECA to create/update this file via `/init` command.

### Commands

![](./images/features/commands.png)

Eca supports commands that usually are triggered via shash (`/`) in the chat, completing in the chat will show the known commands which include ECA commands, MCP prompts and resources.

The built-in commands are:

`/init`: Create/update the AGENTS.md file with details about the workspace for best LLM output quality.
`/login`: Log into a provider. Ex: `/login github-copilot`, `/login anthropic`
`/costs`: Show costs about current session.
`/resume`: Resume a chat from previous session of this workspace folder.
`/config`: Show ECA config for troubleshooting.
`/doctor`: Show information about ECA, useful for troubleshooting.
`/repo-map-show`: Show the current repoMap context of the session.
`/prompt-show`: Show the final prompt sent to LLM with all contexts and ECA details.

#### Custom commands

It's possible to configure custom command prompts, for more details check [its configuration](./configuration.md#custom-command-prompts)

### Login

It's possible to login to some providers using `/login <provider>`, ECA will ask and give instructions on how to authenticate in that provider and save the login info globally in its cache `~/.cache/eca/db.transit.json`.

Current supported providers with login:
- `anthropic`: with options to login to Claude Max/Pro or create API keys.
- `github-copilot`: via Github oauth.

##  Completion

Soon

## Edit 

Soon

