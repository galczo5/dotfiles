function fish_source --description 'Import env changes from a POSIX shell script into fish'
	set -l script $argv[1]
	test -r "$script"; or return 0

	set -l sentinel __SOURCE_SH_SENTINEL__

	# One bash process: dump env, source script, dump env. Split on NUL so
	# values containing newlines survive. A sentinel separates before/after.
	set -l dump (bash -c "
		env -0
		printf '%s\0' '$sentinel'
		source '$script' >/dev/null 2>&1
		env -0
	" | string split0)

	set -l idx (contains -i -- $sentinel $dump)
	test -n "$idx"; or return 1
	set -l before $dump[1..(math $idx - 1)]
	set -l after $dump[(math $idx + 1)..]

	set -l before_keys
	set -l before_vals
	for pair in $before
		set -l kv (string split -m 1 = -- $pair)
		set -a before_keys $kv[1]
		set -a before_vals $kv[2]
	end

	for pair in $after
		set -l kv (string split -m 1 = -- $pair)
		set -l key $kv[1]
		set -l value $kv[2]

		contains -- $key _ SHLVL PWD OLDPWD; and continue

		set -l i (contains -i -- $key $before_keys)
		test -n "$i"; and test "$before_vals[$i]" = "$value"; and continue

		if contains -- $key PATH MANPATH CDPATH INFOPATH
			set -gx $key (string split : -- $value)
		else
			set -gx $key $value
		end
	end
end
