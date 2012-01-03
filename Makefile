
TARGET=/tmp/_oldtype_stable

batch0:
	bash -c "export OT_HOME=`pwd` ; cd bin; ./batch.sh"

dist_stable:
	mkdir -p $(TARGET)
	/bin/cp -r * $(TARGET)/
	cat ./config.sh | sed 's/OT_MASTER=nil/OT_MASTER=t/' | sed 's/site-unstable/site-stable/' | sed 's/newtype/oldtype/' > $(TARGET)/config.sh
	cat ./Kahua/oldtype/Makefile | sed 's/site-unstable/site-stable/' > $(TARGET)/Kahua/oldtype/Makefile
	echo generated stable release to "$(TARGET)"
