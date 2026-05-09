# Source this file, then call:
#   pick "name1|command1" "name2|command2" ...
# Each entry: display name, then `|`, then a shell command (eval'd in the current shell,
# so functions defined in the calling script are available).

pick() {
    local items=("$@")
    local selected=()
    local cursor=0
    local i

    [ "${#items[@]}" -eq 0 ] && return 0
    for i in "${!items[@]}"; do selected[$i]=0; done

    _pick_draw() {
        tput cup 0 0
        printf "↑/↓ move   space toggle   a all   n none   enter run   q quit\n\n"
        for i in "${!items[@]}"; do
            local name="${items[$i]%%|*}"
            local mark
            [ "${selected[$i]}" -eq 1 ] && mark="[x]" || mark="[ ]"
            if [ "$i" -eq "$cursor" ]; then
                printf "\033[7m > %s %s\033[0m\n" "$mark" "$name"
            else
                printf "   %s %s\n" "$mark" "$name"
            fi
        done
        tput ed
    }

    tput smcup
    tput civis
    clear
    trap 'tput rmcup; tput cnorm; exit 130' INT TERM

    local quit=0 key k1 k2
    while true; do
        _pick_draw
        IFS= read -r -s -n1 key
        case "$key" in
            $'\x1b')
                IFS= read -r -s -n1 k1
                IFS= read -r -s -n1 k2
                case "${k1}${k2}" in
                    '[A') ((cursor > 0)) && ((cursor--)) ;;
                    '[B') ((cursor < ${#items[@]} - 1)) && ((cursor++)) ;;
                esac
                ;;
            ' ') [ "${selected[$cursor]}" -eq 0 ] && selected[$cursor]=1 || selected[$cursor]=0 ;;
            'a'|'A') for i in "${!items[@]}"; do selected[$i]=1; done ;;
            'n'|'N') for i in "${!items[@]}"; do selected[$i]=0; done ;;
            'q'|'Q') quit=1; break ;;
            '') break ;;
        esac
    done

    tput rmcup
    tput cnorm
    trap - INT TERM

    [ "$quit" -eq 1 ] && return 0

    for i in "${!items[@]}"; do
        if [ "${selected[$i]}" -eq 1 ]; then
            local name="${items[$i]%%|*}"
            local cmd="${items[$i]#*|}"
            echo "→ $name"
            eval "$cmd"
        fi
    done
}
