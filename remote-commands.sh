#!/bin/bash

# working with WMI Rescue - small Linux image based on 
# Debian distribution; see http://rescue.wmi.amu.edu.pl

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
DEBIAN_PACKAGES_TO_INSTALL="build-essential gfortran ed htop libxml2-dev ca-certificates curl libcurl4-openssl-dev gdebi-core sshpass default-jre default-jdk libpcre3-dev zlib1g-dev liblzma-dev"
SHELL_SCRIPT=$(basename $0)

generate_ssh_keys()
{
    echo "Generating SSH keys"
    mkdir -p ssh
    ssh-keygen -q -t rsa -b 4096 -f ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} -P "" -C "rscript@remote"
    mv ${SSH_KEYS_DIR}/${SSH_KEY_PRIV}.pub ${SSH_KEYS_DIR}/${SSH_KEY_PUB}
}

install_env()
{
    echo "Installing environment"
    apt-get update
    DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confnew" upgrade
    DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confnew" install ${DEBIAN_PACKAGES_TO_INSTALL}
    apt-get clean
}

install_mro()
{
    echo "Installing Microsoft R Open"
    # dependency
    wget http://ftp.debian.org/debian/pool/main/libj/libjpeg8/libjpeg8_8d1-2_amd64.deb
    gdebi -n libjpeg8_8d1-2_amd64.deb
    rm libjpeg8_8d1-2_amd64.deb

    # install Microsoft R Open
    wget https://mran.microsoft.com/install/mro/${MRO_VERSION}/MRO-${MRO_VERSION}-Ubuntu-${MRO_UBUNTU}.x86_64.deb
    gdebi -n MRO-${MRO_VERSION}-Ubuntu-${MRO_UBUNTU}.x86_64.deb
    rm MRO-${MRO_VERSION}-Ubuntu-${MRO_UBUNTU}.x86_64.deb
    
    apt-get clean
}

install_mkl()
{
    echo "Installing Intel MKL"
    wget https://mran.microsoft.com/install/mro/${MRO_VERSION}/RevoMath-${MRO_VERSION}.tar.gz
    tar -xzf RevoMath-${MRO_VERSION}.tar.gz
    cd RevoMath
    sed -i '16,18d' RevoMath.sh
    echo 1 | ./RevoMath.sh
    cd ..
    rm -r RevoMath*
}

install_r_libraries()
{
    echo "Installing R libraries"
    mkdir -p ~/.checkpoint
    Rscript init.R # run checkpoint
}

dump_r_libraries()
{
    echo "Making R libraries dump"
    wd=`pwd`
    cd ~/
    tar -czf $wd/checkpoint.tar.gz .checkpoint/*
    cd $wd   
}

dump_mkl()
{
    echo "Making Intel MKL files dump"
    tar -czf RevoMath.tar.gz /usr/lib64/MRO-${MRO_VERSION}/R-${MRO_VERSION}/lib/R/lib/* /usr/lib64/MRO-${MRO_VERSION}/R-${MRO_VERSION}/lib/R/library/RevoUtilsMath/*
}

hosts_push_ssh_key()
{
    echo "Pushing SSH keys to hosts"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "mkdir-ssh "
        sshpass -p ${SSHPASS_PWD} ssh ${SSH_OPTIONS} ${SSH_USER}@${host} 'mkdir -p ~/.ssh'
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "push-key "
        sshpass -p ${SSHPASS_PWD} scp ${SSH_OPTIONS} ${SSH_KEYS_DIR}/${SSH_KEY_PUB} ${SSH_USER}@${host}:~/.ssh
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "append-key "
        sshpass -p ${SSHPASS_PWD} ssh ${SSH_OPTIONS} ${SSH_USER}@${host} "cat ~/.ssh/${SSH_KEY_PUB} >> ~/.ssh/authorized_keys"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "disable-password-login "
        sshpass -p ${SSHPASS_PWD} ssh ${SSH_OPTIONS} ${SSH_USER}@${host} "sed -i -e 's/#PasswordAuthentication yes/PasswordAuthentication no/ig' /etc/ssh/sshd_config; service ssh restart"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

hosts_push_r_libraries_dump()
{
    echo "Pushing R libraries dump to hosts"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "push-file "
        scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} checkpoint.tar.gz ${SSH_USER}@${host}:~/
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "unpack-file "
        ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "tar -xzf checkpoint.tar.gz -C ~/; rm checkpoint.tar.gz"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

hosts_push_mkl_dump()
{
    echo "Pushing Intel MKL files dump to hosts"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "push-file "
        scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} RevoMath.tar.gz ${SSH_USER}@${host}:~/
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "unpack-file "
        ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "tar -xzf RevoMath.tar.gz -C /; rm RevoMath.tar.gz"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

hosts_push_project_r_files()
{
    echo "Pushing project R files to hosts"
    
    tar -czf project-r-files.tar.gz *.R *.R.user
    
    while read host; do
        echo -n "${host} ... "
        
        echo -n "push-file "
        scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} project-r-files.tar.gz ${SSH_USER}@${host}:~/
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "unpack-file "
        ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "tar -xzf project-r-files.tar.gz -C ~/; rm project-r-files.tar.gz"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
    
    rm project-r-files.tar.gz
}

hosts_push_shell_script()
{
    echo "Pushing shell script to hosts"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "push-file "
        scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SHELL_SCRIPT} ${SSH_USER}@${host}:~/
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

hosts_install()
{
    case "$1" in
        "env")          echo "Installing environment on hosts" ;;
        "mro")          echo "Installing Microsoft R Open on hosts" ;;
        "mkl")          echo "Installing Intel MKL on hosts" ;;
        "r_libraries")  echo "Installing R libraries on hosts" ;;
        *)              echo "Unknown remote install command"; exit 1
    esac

    while read host; do
        echo -n "${host} ... "
        
        echo "script-run"
        { ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "bash ${SHELL_SCRIPT} install_$1 &> install_$1.log" ;
          echo "[$(date +%T)] ${host} finished, $(jobs -rp | wc -l) hosts running" ; } &

    done < ${HOSTS_FILE}
    
    echo "Waiting for $(jobs -rp | wc -l) hosts"
   
    while true; do
        if [ $(jobs -rp | wc -l) -eq 0 ] ; then break; fi
        sleep 1
    done
}

hosts_install_env()         { hosts_install env; }
hosts_install_mro()         { hosts_install mro; }
hosts_install_mkl()         { hosts_install mkl; }
hosts_install_r_libraries() { hosts_install r_libraries; }

hosts_power_off()
{
    echo "Power off on hosts"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "power-off "
        ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "poweroff"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

make_remote_connection_list()
{
    echo -n "Making remote connection list: "
    case "$1" in
        "single")
            echo "one connection per host"
            cat ${HOSTS_FILE} > ${CONNECTION_LIST_FILE}
            ;;
        "nproc")
            echo "'number of cores' per host" 
            echo "" > ${CONNECTION_LIST_FILE}
            while read host; do
                echo -n "${host} ... "
                
                echo -n "rscript "
                cornum=`ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} '/usr/bin/Rscript -e "cat(parallel::detectCores())"'`
                ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
                
                if ! [[ $cornum =~ '^[0-9]+$' ]] ; then
                    echo "R error: $cornum"
                else
                    echo "- $cornum cores"
                    for i in {1..10}; do echo $cornum >> ${CONNECTION_LIST_FILE}; done
                fi
            done < ${HOSTS_FILE}
            ;;
        *) 
            echo "unknown type"
            exit 2
    esac
}

make_remote_connection_list_single() { make_remote_connection_list single; }
make_remote_connection_list_nproc()  { make_remote_connection_list nproc; }

for i in "$@"
do
    case "$i" in
        "hosts_install") ;;
        "make_remote_connection_list") ;;
        *) $i
    esac
done
