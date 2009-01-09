
batch0:
	bash -c "export OT_HOME=`pwd` ; cd bin; ./batch.sh"

stable:
	tar cf  ../stable.tar --exclude .svn * 
	tar xfC ../stable.tar ../branches/stable
	/bin/rm -f ../stable.tar
	cat ./config.sh | sed 's/OT_MASTER=nil/OT_MASTER=t/' | sed 's/site-unstable/site-stable/' | sed 's/newtype/oldtype/' > ../branches/stable/config.sh
	cat ./Kahua/oldtype/Makefile | sed 's/site-unstable/site-stable/' > ../branches/stable/Kahua/oldtype/Makefile

merge:
	bash -c "cd .. ; svn merge tags/0.3.7@ trunk@ branches/stable"
	cat ./config.sh | sed 's/OT_MASTER=nil/OT_MASTER=t/' | sed 's/site-unstable/site-stable/' | sed 's/newtype/oldtype/' > ../branches/stable/config.sh
	cat ./Kahua/oldtype/Makefile | sed 's/site-unstable/site-stable/' > ../branches/stable/Kahua/oldtype/Makefile
