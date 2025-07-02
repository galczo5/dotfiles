function fish_greeting
end

function mc
	env SHELL=/bin/bash command mc $argv
end

function fish_prompt 

	set -l branch ""
	if git rev-parse --is-inside-work-tree >/dev/null 2>&1
		set branch (set_color yellow --bold)  (git symbolic-ref --short HEAD 2>/dev/null; or git rev-parse --short HEAD 2>/dev/null)(set_color normal)
	end

	set -l branch (echo $branch | tr -d '\n')

    set -l currentPath (set_color blue --bold)(prompt_pwd)(set_color normal)
	set -l currentUser (set_color blue --bold)$USER(set_color normal)

	printf '%s %s%s ' $currentUser $currentPath $branch 

end
