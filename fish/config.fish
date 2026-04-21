set -gx XDG_CONFIG_HOME "$HOME/.config"

fish_source ~/.zprofile
fish_source ~/.zshrc

fish_add_path "$HOME/.local/bin"
fish_add_path /opt/homebrew/bin

fnm env --use-on-cd --shell fish | source

set fish_cursor_default block

function fish_greeting
end

function mc
	env SHELL=/bin/bash command mc $argv
end

function fish_prompt
	printf '\e[2 q'

	set -l branch ""
	if git rev-parse --is-inside-work-tree >/dev/null 2>&1
		set branch (set_color red --bold) (git symbolic-ref --short HEAD 2>/dev/null; or git rev-parse --short HEAD 2>/dev/null)(set_color normal)
	end

	set -l branch (echo $branch | tr -d '\n')

    set -l currentPath (set_color blue --bold)(prompt_pwd)(set_color normal)

	printf '%s %s%s ' $currentPath $branch

end
