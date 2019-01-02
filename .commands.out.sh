set -e;

_subroutine_a () {
    stack "build" "file-server:lib"
}

_subroutine_b () {
    stack "build" ":file-server"
    stack "exec" "api-server"
}

_subroutine_c () {
    stack "ghci" "file-server:lib"
}

_subroutine_d () {
    stack "build" ":file-server"
    stack "install"
}

build___library () {
    _subroutine_a
}

run () {
    _subroutine_b
}

ghci () {
    _subroutine_c
}

install () {
    _subroutine_d
}


case $1 in
    ("build::library") build___library ;;
    ("ghci") ghci ;;
    ("install") install ;;
    ("run") run ;;
esac
