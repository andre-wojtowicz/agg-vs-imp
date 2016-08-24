#!/bin/bash

# working with WMI Rescue - small Linux image based on 
# Debian distribution; see http://rescue.wmi.amu.edu.pl

# config

MRO_VERSION="3.3.0"
MRO_UBUNTU="14.4"
SSH_OPTIONS="-o ConnectTimeout=5 -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no -q"
SSH_USER="root"
SSHPASS_PWD="wmi"
SSH_KEYS_DIR="ssh"
SSH_KEY_PRIV="rsa-priv.key"
SSH_KEY_PUB="rsa-pub.key"
HOSTS_FILE="remote-hosts.txt"
CONNECTION_LIST_FILE="remote-connection-list.txt"
DEBIAN_PACKAGES_TO_INSTALL="build-essential gfortran ed htop libxml2-dev ca-certificates curl libcurl4-openssl-dev gdebi-core sshpass default-jre default-jdk libpcre3-dev zlib1g-dev liblzma-dev libbz2-dev"
REMOTE_DETECT_LOGICAL_CPUS="FALSE"

SHELL_SCRIPT=$(basename $0)
LOG_STEPS="logs/${SHELL_SCRIPT%.*}".log
HOSTS_ARRAY=()

# messaging

report_error()
{
    echo $1 > /tmp/command_error.$$
}

[[ -w /tmp ]] && report_error 0

# https://stackoverflow.com/a/5196220
# modified for Debian

# Use step(), try(), and next() to perform a series of commands and print
# [  OK  ] or [FAILED] at the end. The step as a whole fails if any individual
# command fails.
#
# Example:
#     step "Remounting / and /boot as read-write:"
#     try mount -o remount,rw /
#     try mount -o remount,rw /boot
#     next
step()
{
    echo -n "* $@   "

    STEP_OK=0
    [[ -w /tmp ]] && echo $STEP_OK > /tmp/step.$$
}

try()
{
    # skip if previuos command in step ended with succes
    [[ -f /tmp/step.$$ ]] && { PREV_STEP=$(< /tmp/step.$$); }
    [[ $PREV_STEP -ne 0 ]] && return 1

    # Check for `-b' argument to run command in the background.
    local BG=

    [[ $1 == -b ]] && { BG=1; shift; }
    [[ $1 == -- ]] && {       shift; }

    # Run the command.
    if [[ -z $BG ]]; then
        "$@"
    else
        "$@" &
    fi

    # Check if command failed and update $STEP_OK if so.
    local EXIT_CODE=$?

    if [[ $EXIT_CODE -ne 0 ]]; then
        STEP_OK=$EXIT_CODE
        [[ -w /tmp ]] && echo $STEP_OK > /tmp/step.$$

        if [[ -n $LOG_STEPS ]]; then
            local FILE=$(readlink -m "${BASH_SOURCE[1]}")
            local LINE=${BASH_LINENO[0]}

            echo "$FILE: line $LINE: Command \`$*' failed with exit code $EXIT_CODE." >> "$LOG_STEPS"
        fi
    fi

    return $EXIT_CODE
}

test -t 1 && CONSOLE_STDOUT=1 || CONSOLE_STDOUT=0
[[ $TERM != "dumb" ]] && [[ $CONSOLE_STDOUT -eq 1 ]] && CONSOLE_COLORS=1 || CONSOLE_COLORS=0

[ $CONSOLE_COLORS -eq 1 ] && CONSOLE_RED=$(tput setaf 1)
[ $CONSOLE_COLORS -eq 1 ] && CONSOLE_GREEN=$(tput setaf 2)
[ $CONSOLE_COLORS -eq 1 ] && CONSOLE_YELLOW=$(tput setaf 3)
[ $CONSOLE_COLORS -eq 1 ] && CONSOLE_NORMAL=$(tput sgr0)
[ $CONSOLE_COLORS -eq 1 ] && CONSOLE_RESULT_POS=$[$(tput cols)-10] || CONSOLE_RESULT_POS=0

next()
{
    # https://stackoverflow.com/a/5506264
    [ $CONSOLE_COLORS -eq 1 ] && tput hpa ${CONSOLE_RESULT_POS}

    [[ -f /tmp/step.$$ ]] && { STEP_OK=$(< /tmp/step.$$); rm -f /tmp/step.$$; }
    [[ $STEP_OK -eq 0 ]]  && printf '%s%s' "$CONSOLE_GREEN" "[ OK ]" "$CONSOLE_NORMAL" || printf '%s%s' "$CONSOLE_RED" "[FAIL]" "$CONSOLE_NORMAL"
    echo
    
    [[ $STEP_OK -ne 0 ]] && report_error 1

    return $STEP_OK
}

info()
{
    echo -n "* $@   "
    [ $CONSOLE_COLORS -eq 1 ] && tput hpa ${CONSOLE_RESULT_POS}
    printf '%s' "[INFO]"
    echo
}

fail()
{
    echo -n "* $@   "
    [ $CONSOLE_COLORS -eq 1 ] && tput hpa ${CONSOLE_RESULT_POS}
    printf '%s%s' "$CONSOLE_RED" "[FAIL]" "$CONSOLE_NORMAL"
    echo
}

success()
{
    echo -n "* $@   "
    [ $CONSOLE_COLORS -eq 1 ] && tput hpa ${CONSOLE_RESULT_POS}
    printf '%s%s' "$CONSOLE_GREEN" "[ OK ]" "$CONSOLE_NORMAL"
    echo
}

warn()
{
    echo -n "* $@   "
    [ $CONSOLE_COLORS -eq 1 ] && tput hpa ${CONSOLE_RESULT_POS}
    printf '%s%s' "$CONSOLE_YELLOW" "[WARN]" "$CONSOLE_NORMAL"
    echo
}

# functions

generate_ssh_keys()
{
    step "Generating SSH keys"
    try mkdir -p ssh
    try ssh-keygen -q -t rsa -b 4096 -f ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} -P "" -C "rscript@remote"
    try mv ${SSH_KEYS_DIR}/${SSH_KEY_PRIV}.pub ${SSH_KEYS_DIR}/${SSH_KEY_PUB}
    next
    check_if_command_error
}

install_env()
{
    step "Installing environment"
    echo
    try apt-get update
    local DEBIAN_FRONTEND=noninteractive
    try apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confnew" upgrade
    try apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confnew" install ${DEBIAN_PACKAGES_TO_INSTALL}
    try apt-get clean
    next
    check_if_command_error
}

install_mro()
{
    step "Installing Microsoft R Open"
    echo
    # dependency
    try wget http://ftp.debian.org/debian/pool/main/libj/libjpeg8/libjpeg8_8d1-2_amd64.deb
    try gdebi -n libjpeg8_8d1-2_amd64.deb
    rm -f libjpeg8_8d1-2_amd64.deb

    # install Microsoft R Open
    try wget https://mran.microsoft.com/install/mro/${MRO_VERSION}/MRO-${MRO_VERSION}-Ubuntu-${MRO_UBUNTU}.x86_64.deb
    try gdebi -n MRO-${MRO_VERSION}-Ubuntu-${MRO_UBUNTU}.x86_64.deb
    rm -f MRO-${MRO_VERSION}-Ubuntu-${MRO_UBUNTU}.x86_64.deb
    
    try apt-get clean
    next
    check_if_command_error
}

install_mkl()
{
    step "Installing Intel MKL"
    echo
    try wget https://mran.microsoft.com/install/mro/${MRO_VERSION}/RevoMath-${MRO_VERSION}.tar.gz
    try tar -xzf RevoMath-${MRO_VERSION}.tar.gz
    try cd RevoMath
    try sed -i '16,18d' RevoMath.sh
    try echo 1 | ./RevoMath.sh
    try cd ..
    rm -rf RevoMath* 
    next
    check_if_command_error
}

install_r_libraries()
{
    step "Installing R libraries"
    echo
    try mkdir -p ~/.checkpoint
    try Rscript init.R # run checkpoint
    next
    check_if_command_error
}

dump_project_r_files()
{
    step "Making project R files dump"
    try tar -czf project-r-files.tar.gz *.R*
    next
    check_if_command_error
}

dump_r_libraries()
{
    step "Making R libraries dump"
    wd=`pwd`
    cd ~/
    try tar -czf $wd/checkpoint.tar.gz .checkpoint/*
    cd $wd
    next
    check_if_command_error
}

hosts_push_ssh_key()
{
    info "Pushing SSH keys to hosts"
    for host in "${HOSTS_ARRAY[@]}"; do
        step "-- ${host}"
        try sshpass -p ${SSHPASS_PWD} ssh ${SSH_OPTIONS} ${SSH_USER}@${host} 'mkdir -p ~/.ssh'
        try sshpass -p ${SSHPASS_PWD} scp ${SSH_OPTIONS} ${SSH_KEYS_DIR}/${SSH_KEY_PUB} ${SSH_USER}@${host}:~/.ssh
        try sshpass -p ${SSHPASS_PWD} ssh ${SSH_OPTIONS} ${SSH_USER}@${host} "cat ~/.ssh/${SSH_KEY_PUB} >> ~/.ssh/authorized_keys"
        try sshpass -p ${SSHPASS_PWD} ssh ${SSH_OPTIONS} ${SSH_USER}@${host} "sed -i -e 's/#PasswordAuthentication yes/PasswordAuthentication no/ig' /etc/ssh/sshd_config; service ssh restart"
        next
    done
    check_if_command_error
}

hosts_push_r_libraries_dump()
{
    info "Pushing R libraries dump to hosts"
    for host in "${HOSTS_ARRAY[@]}"; do
        step "-- ${host}"
        try scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} checkpoint.tar.gz ${SSH_USER}@${host}:~/
        try ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "tar -xzf checkpoint.tar.gz -C ~/; rm checkpoint.tar.gz"
        next
    done
    check_if_command_error
}

hosts_push_project_r_files()
{
    info "Pushing project R files to hosts"
    for host in "${HOSTS_ARRAY[@]}"; do
        step "-- ${host}"
        try scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} project-r-files.tar.gz ${SSH_USER}@${host}:~/
        try ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "tar -xzf project-r-files.tar.gz -C ~/; rm project-r-files.tar.gz"
        next
    done
    check_if_command_error
}

hosts_push_shell_script()
{
    info "Pushing shell script to hosts"
    for host in "${HOSTS_ARRAY[@]}"; do
        step "-- ${host}"
        try scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SHELL_SCRIPT} ${SSH_USER}@${host}:~/
        try ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "touch ${HOSTS_FILE}"
        next
    done
    check_if_command_error
}

hosts_install()
{
    case "$1" in
        "env")          info "Installing environment on hosts" ;;
        "mro")          info "Installing Microsoft R Open on hosts" ;;
        "mkl")          info "Installing Intel MKL on hosts" ;;
        "r_libraries")  info "Installing R libraries on hosts" ;;
        *)              fail "Unknown remote install command"; report_error 1; check_if_command_error
    esac

    for host in "${HOSTS_ARRAY[@]}"; do
        info "-- Invoking ${host}"
        
        {   ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "bash ${SHELL_SCRIPT} install_$1 &> install_$1.log" ;
            endcode=$?
            if [ $endcode -eq 0 ] ; then
                success "-- ${host} finished, $(jobs -rp | wc -l) hosts running"
            else
                fail "-- ${host} finished, $(jobs -rp | wc -l) hosts running"
                report_error 1
            fi
        } &

    done
    
    info "- Waiting for $(jobs -rp | wc -l) hosts"
   
    while true; do
        if [ $(jobs -rp | wc -l) -eq 0 ] ; then break; fi
        sleep 1
    done
    
    check_if_command_error
}

hosts_install_env()         { hosts_install env; }
hosts_install_mro()         { hosts_install mro; }
hosts_install_mkl()         { hosts_install mkl; }
hosts_install_r_libraries() { hosts_install r_libraries; }

hosts_power_off()
{
    info "Power off on hosts"
    for host in "${HOSTS_ARRAY[@]}"; do
        step "-- ${host}"
        try ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "poweroff"
        next
    done
    check_if_command_error
}

make_remote_connection_list()
{
    info "Making remote connection list:"
    if [ -f ${CONNECTION_LIST_FILE} ] ; then rm ${CONNECTION_LIST_FILE}; fi
    case "$1" in
        "single")
            step "one connection per host"
            for host in "${HOSTS_ARRAY[@]}"; do
                try echo ${host} >> ${CONNECTION_LIST_FILE}
            done
            next
            ;;
        "nproc")
            info "'number of cores' per host" 
            for host in "${HOSTS_ARRAY[@]}"; do
                step "-- ${host}"
                cornum=`try ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} '/usr/bin/Rscript -e "cat(parallel::detectCores(logical = ${REMOTE_DETECT_LOGICAL_CPUS}))"'`
                
                regex='^[0-9]+$'
                if ! [[ $cornum =~ $regex ]] ; then
                    try false
                else
                    for ((i=1; i<=$cornum; i++)); do try echo ${host} >> ${CONNECTION_LIST_FILE}; done
                    echo -n "($cornum cores)   "
                fi
                next
            done
            ;;
        *) 
            fail "unknown type"
            report_error 1
    esac
    check_if_command_error
}

make_remote_connection_list_single() { make_remote_connection_list single; }
make_remote_connection_list_nproc()  { make_remote_connection_list nproc; }

hosts_check_install_log()
{    
    info "Checking install log on hosts"
    for host in "${HOSTS_ARRAY[@]}"; do
        step "-- ${host}"
        echo
        try ssh ${SSH_OPTIONS/-q/} -o LogLevel=error -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "cat install_$1.log"
        next
    done
    check_if_command_error
}

hosts_check_install_log_env()         { hosts_check_install_log env; }
hosts_check_install_log_mro()         { hosts_check_install_log mro; }
hosts_check_install_log_mkl()         { hosts_check_install_log mkl; }
hosts_check_install_log_r_libraries() { hosts_check_install_log r_libraries; }

hosts_check_worker_log()
{
    info "Checking worker log on hosts"
    for host in "${HOSTS_ARRAY[@]}"; do
        step "-- ${host}"
        echo
        try ssh ${SSH_OPTIONS/-q/} -o LogLevel=error -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "for f in worker-remote-*.log; do echo \$f; cat -n \$f; done"
        next
    done
    check_if_command_error
}

hosts_clean_worker_log()
{
    info "Cleaning workers logs"
    for host in "${HOSTS_ARRAY[@]}"; do
        step "-- ${host}"
        try ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "rm -f worker-remote-*.log"
        next
    done
    check_if_command_error
}

check_if_command_error()
{
    errcode=$(< /tmp/command_error.$$)
    [[ $errcode -ne 0 ]] && { warn "Stopping script execution"; rm -f /tmp/command_error.$$; exit $errcode; }
}

my_configure_hosts()
{
    #generate_ssh_keys
    hosts_push_ssh_key
    hosts_push_shell_script
    dump_project_r_files
    hosts_push_project_r_files
    hosts_install_env
    hosts_install_mro
    #hosts_install_mkl
    #hosts_install_r_libraries
        hosts_push_r_libraries_dump
    make_remote_connection_list_nproc
        #make_remote_connection_list_single
}

configure_hosts()
{
    generate_ssh_keys
    hosts_push_ssh_key
    hosts_push_shell_script
    dump_project_r_files
    hosts_push_project_r_files
    hosts_install_env
    hosts_install_mro
    #hosts_install_mkl
    hosts_install_r_libraries
        #hosts_push_r_libraries_dump
    make_remote_connection_list_nproc
        #make_remote_connection_list_single
}

# read hosts from file or stdin

if [ -t 0 ]; then
    if [ ! -f "$HOSTS_FILE" ]
    then
        fail "Hosts file $HOSTS_FILE does not exists"
        report_error 74
        check_if_command_error
    fi
    readarray -t HOSTS_ARRAY < $HOSTS_FILE
else
    while read -r host ; do
        HOSTS_ARRAY+=("$host")
    done
fi

info "Working with ${#HOSTS_ARRAY[@]} hosts"

# read arguments as commands

for i in "$@"
do
    case "$i" in
        "hosts_install"|"hosts_check_install_log"|"make_remote_connection_list"|"info"|"fail"|"success"|"warn"|"next"|"try"|"step") ;;
        *) 
            if [ "$(type -t $i)" = "function" ]; then 
                $i
            else
                fail "Command $i not found"
                report_error 127
            fi
    esac
    
    check_if_command_error
done

rm -f /tmp/command_error.$$
