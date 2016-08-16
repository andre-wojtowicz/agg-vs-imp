#!/bin/bash

# working with WMI Rescue - small Linux image based on 
# Debian distribution; see http://rescue.wmi.amu.edu.pl

MRO_VERSION="3.2.5"
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

function generate_ssh_keys
{
    echo "Generating SSH keys"
    mkdir -p ssh
    ssh-keygen -q -t rsa -b 4096 -f ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} -P "" -C "rscript@remote"
    mv ${SSH_KEYS_DIR}/${SSH_KEY_PRIV}.pub ${SSH_KEYS_DIR}/${SSH_KEY_PUB}
}

function install_env
{
    echo "Installing environment"
    apt-get update
    DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confnew" upgrade
    DEBIAN_FRONTEND=noninteractive apt-get -y -o Dpkg::Options::="--force-confdef" -o Dpkg::Options::="--force-confnew" install ${DEBIAN_PACKAGES_TO_INSTALL}
}

function install_mro
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
}

function install_mkl
{
    echo "Installing Intel MKL"
    wget https://mran.microsoft.com/install/mro/${MRO_VERSION}/RevoMath-${MRO_VERSION}.tar.gz
    tar -xvzf RevoMath-${MRO_VERSION}.tar.gz
    cd RevoMath
    sed -i '16,18d' RevoMath.sh
    echo 1 | ./RevoMath.sh
    cd ..
    rm -r RevoMath*
}

function install_r_libraries
{
    echo "Installing R libraries"
    mkdir -p ~/.checkpoint
    Rscript init.R # run checkpoint
}

function dump_r_libraries
{
    echo "Making R libraries dump"
    wd=`pwd`
    cd ~/
    tar -cvzf $wd/checkpoint.tar.gz .checkpoint/*
    cd $wd   
}

function dump_mkl
{
    echo "Making Intel MKL files dump"
    tar -cvzf RevoMath.tar.gz /usr/lib64/MRO-${MRO_VERSION}/R-${MRO_VERSION}/lib/R/lib/* /usr/lib64/MRO-${MRO_VERSION}/R-${MRO_VERSION}/lib/R/library/RevoUtilsMath/*
}

function slaves_push_ssh_key 
{
    echo "Pushing SSH keys to slaves"
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

function slaves_push_r_libraries_dump
{
    echo "Pushing R libraries dump to slaves"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "push-file "
        scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} checkpoint.tar.gz ${SSH_USER}@${host}:~/
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "unpack-file "
        ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "tar -xvzf checkpoint.tar.gz -C ~/; rm checkpoint.tar.gz"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

function slaves_push_mkl_dump
{
    echo "Pushing Intel MKL files dump to slaves"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "push-file "
        scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} RevoMath.tar.gz ${SSH_USER}@${host}:~/
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "unpack-file "
        ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "tar -xvzf RevoMath.tar.gz -C /; rm RevoMath.tar.gz"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

function slaves_push_project_r_files
{
    echo "Pushing project R files"
    
    tar -cvzf project-r-files.tar.gz *.R *.R.user
    
    while read host; do
        echo -n "${host} ... "
        
        echo -n "push-file "
        scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} project-r-files.tar.gz ${SSH_USER}@${host}:~/
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo -n "unpack-file "
        ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "tar -xvzf project-r-files.tar.gz -C ~/; rm project-r-files.tar.gz"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
    
    rm project-r-files.tar.gz
}

function slaves_push_shell_script
{
    echo "Pushing shell script"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "push-file "
        scp ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SHELL_SCRIPT} ${SSH_USER}@${host}:~/
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

function slaves_install_env
{
    echo "Installing environment on slaves"
    while read host; do
        echo -n "${host} ... "
        
        
        
        echo "ok"
    done < ${HOSTS_FILE}
}

function slaves_install_mro
{
    echo "Installing Microsoft R Open on slaves"
    while read host; do
        echo -n "${host} ... "
        
        
        
        echo "ok"
    done < ${HOSTS_FILE}
}

function slaves_install_mkl
{
    echo "Installing Intel MKL on slaves"
    while read host; do
        echo -n "${host} ... "
        
        
        
        echo "ok"
    done < ${HOSTS_FILE}
}

function slaves_install_r_libraries
{
    echo "Installing R libraries on slaves"
    while read host; do
        echo -n "${host} ... "
        
        
        
        echo "ok"
    done < ${HOSTS_FILE}
}

function slaves_power_off
{
    echo "Power off on slaves"
    while read host; do
        echo -n "${host} ... "
        
        echo -n "power-off "
        ssh ${SSH_OPTIONS} -i ${SSH_KEYS_DIR}/${SSH_KEY_PRIV} ${SSH_USER}@${host} "poweroff"
        ret=$?; if [ $ret -ne 0 ] ; then echo "error $ret"; continue; fi
        
        echo "ok"
    done < ${HOSTS_FILE}
}

for i in "$@"
do
    $i
done
