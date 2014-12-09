yum install -y avahi

HOSTNAME=centostest.localdomain
echo "NETWORKING=yes
HOSTNAME=$HOSTNAME">/etc/sysconfig/network
hostname $HOSTNAME

service messagebus start
service avahi-daemon start
