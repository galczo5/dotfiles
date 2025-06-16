function fish_greeting
end

function mc
	env SHELL=/bin/bash command mc $argv
end

function fish_prompt 

	set -l branch ""
	if git rev-parse --is-inside-work-tree >/dev/null 2>&1
		set branch (set_color blue --bold --italics)(git symbolic-ref --short HEAD 2>/dev/null; or git rev-parse --short HEAD 2>/dev/null)(set_color normal)
	end

	set -l branch (echo $branch | tr -d '\n')
	set -l now (set_color yellow)(date "+%H:%M:%S")(set_color normal)

	printf '%s %s %s > ' $USER (prompt_pwd) $branch

end
