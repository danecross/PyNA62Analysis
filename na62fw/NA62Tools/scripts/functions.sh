pathappend() {
    if [ -z "${!1}" ]; then # Make sure the variable exists and is exported
        eval "${1}="""
    fi
    for ARG in "${@:2}"
    do
        if [[ ":${ARG}:" != *":${!1}:"* ]]; then
            eval "${1}=$ARG:${!1}"
        fi
    done
    export ${1}
}
