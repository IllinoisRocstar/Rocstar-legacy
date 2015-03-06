SHELL		= /bin/sh

CGALROOT	= $(HOME)/projects/CGAL-2.2
ARCH		= i686_Linux-2.4.2-2_g++-2.96.
CGAL_MAKEFILE 	= $(CGALROOT)/make/makefile_$(ARCH)
include $(CGAL_MAKEFILE)

#---------------------------------------------------------------------#
#                    compiler flags
#---------------------------------------------------------------------#

CXXFLAGS = $(CGAL_CXXFLAGS)
CDEPFLAGS = -M

#---------------------------------------------------------------------#
#                    target entries
#---------------------------------------------------------------------#

update: update_headers update_srcs

update_headers:
	@-if [ ! -d ../CGAL ] ; then mkdir ../CGAL ; fi
	@-if [ ! -d ../CGAL/include ] ; then mkdir ../CGAL/include ; fi
	@-if [ -f hdr_files ]; then mv hdr_files hdr_files.bak; fi
	@$(CGAL_CXX) $(CDEPFLAGS) $(CPPFLAGS) $(CXXFLAGS) -c dep_helper.C |  \
		grep CGAL | sed -e 's/\(.*\)include\/CGAL/CGAL/' -e 's/\\//' | \
		sort > hdr_files
	@tar cf - -C $(CGALROOT)/include `cat hdr_files` | \
		(cd ../CGAL/include; tar xvf -)
	@echo
	@echo ===Updated `wc -l hdr_files`! The file names are logged in hdr_files. \n

update_srcs:
	@-if [ ! -d ../CGAL ] ; then mkdir ../CGAL ; fi
	@-if [ ! -d ../CGAL/src ] ; then mkdir ../CGAL/src ; fi
	@-if [ -f src_files ] ; then rm src_files ; fi ; touch src_files
	@-if [ -f hdr_files ]; then \
		for src in `find $(CGALROOT)/src -name "*.C" | \
			sed -e 's/\(.*\)src\///' | sort`; do \
		   	grep -q -i `echo $${src} | sed -e 's/\.C/\.h/'` hdr_files && \
			(tar cf - -C $(CGALROOT)/src $${src} | (cd ../CGAL/src; tar xvf -)) && \
			echo $${src} >> src_files || echo > /dev/null; \
		done \
	fi	
	@echo
	@echo ===Updated `wc -l src_files`! The file names are logged in src_files.
