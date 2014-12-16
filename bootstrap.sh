# Without disabling iptables avahi doesn't work properly. See
# https://bugzilla.redhat.com/show_bug.cgi?id=704809#c9
chkconfig --del iptables
service iptables stop

yum install -y avahi

HOSTNAME=${1%%.*}.localdomain
echo "NETWORKING=yes
HOSTNAME=$HOSTNAME">/etc/sysconfig/network
hostname $HOSTNAME

service messagebus start
service avahi-daemon start
