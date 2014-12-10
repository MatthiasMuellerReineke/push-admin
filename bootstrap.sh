yum install -y avahi

HOSTNAME=${1%%.*}.localdomain
echo "NETWORKING=yes
HOSTNAME=$HOSTNAME">/etc/sysconfig/network
hostname $HOSTNAME

service messagebus start
service avahi-daemon start
