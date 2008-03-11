
batch0:
	bash -c "export OT_HOME=`pwd` ; cd bin; ./batch.sh"

stable:
	tar cf  ../stable.tar --exclude .svn * 
	tar xfC ../stable.tar ../branches/stable
	/bin/rm -f ../stable.tar
