# Copyright (c) 2000, 2001, Red Hat, Inc.
# 
# This file is part of Source-Navigator.
# 
# Source-Navigator is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as published
# by the Free Software Foundation; either version 2, or (at your option)
# any later version.
# 
# Source-Navigator is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along
# with Source-Navigator; see the file COPYING.  If not, write to
# the Free Software Foundation, 59 Temple Place - Suite 330, Boston,
# MA 02111-1307, USA.
# 
################################################
##
## Makefile generation for SN-IDE.
## Copyright (c) 1999 Cygnus Solutions, Inc.
## Written by Ian Roxborough <irox@cygnus.com>
##
################################################

itcl::class MakefileGen {

    # public data members
    public variable TargetName ""
    # The build target name

    # private date members
    protected variable b_target ""
    # Build target object
    protected variable toolchain ""
    # Use this toolchain


    method constructor {target_name {tool_chain ""} {args "" }} {

        set TargetName ${target_name}
        set toolchain ${tool_chain}

    }

    method GenerateMakefile {{filename ""}} {

        #TODO: CheckValidBuildTarget


        # Inialize and load build target data.

        set b_target [snBuildTarget tmptar]
        ${b_target} LoadData ${TargetName}

        DummyCreateToolChain_spec


        if {${toolchain}!=""} {
            #TODO: select toolchain
        } else {
            set toolchainname [${b_target} GetToolChain]
            set toolchain [GetToolChainObject ${toolchainname}]
        }


        if {${filename}==""} {
            # We have been passed a blank files so let generate one
            set dirname [${b_target} GetBuildDirectory]

            # If the build directory does not exist, create it now
            if {![file exists ${dirname}]} {
                if {[catch {file mkdir ${dirname}}]!=0} {

                    # We couldn't create the directory.
                    sn_error_dialog "The Directory \"${dirname}\" cannot be\
                      created."

                    # Delete temp build target.
                    itcl::delete object ${b_target}

                    # Return the $dirname, everything else should unwind safely.
                    return ${dirname}
                }
            }

            set filename [file join ${dirname} snMakefile]
        }


        if {[file exists ${filename}]} {
            file delete ${filename}
        }

        set fd [open ${filename} "CREAT RDWR"]
        if {${fd}==0} {
            # OH no we've got a problem
            # TODO: error reporting
        }

        # Give the Makefile a header (target name, time, date, etc)
        WriteHeader ${fd}

        WriteSUFFIXES ${fd}

        WriteVPATHs ${fd}

        WriteMarcoTool ${fd}
        WriteMarcoFLAGs ${fd}
        WriteMarcoINCLUDES ${fd}
        WriteMarcoDEFINES ${fd}
        WriteMarcoLIBS ${fd}
        WriteMacroLINKER ${fd}

        WriteMacroOBJFILES ${fd}

        WriteRuleALL ${fd}

        WriteRuleTarget ${fd}

        WritePostLinkRule ${fd}

        WriteRuleBuilds ${fd}

        WriteRuleDependences ${fd}

        WriteRuleclean ${fd}

        close ${fd}

        # Delete temp build target
        itcl::delete object ${b_target}

        return ${filename}
    }


    method Test1 {channel tool_chain build_target} {

        set toolchain ${tool_chain}
        set b_target ${build_target}
        set fd ${channel}

        # Give the Makefile a header (target name, time, date, etc)
        puts ${fd} "<<< Testing WriteHeader >>>"
        flush ${fd}
        WriteHeader ${fd}

        puts ${fd} "<<< Testing WriteMarcoTool >>>"
        flush ${fd}
        WriteMarcoTool ${fd}

        puts ${fd} "<<< Testing WriteMarcoFLAGs >>>"
        flush ${fd}
        WriteMarcoFLAGs ${fd}

        puts ${fd} "<<< Testing WriteMarcoINCLUDES >>>"
        flush ${fd}
        WriteMarcoINCLUDES ${fd}

        puts ${fd} "<<< Testing WriteMarcoDEFINES >>>"
        flush ${fd}
        WriteMarcoDEFINES ${fd}

        puts ${fd} "<<< Testing WriteMarcoLIBS >>>"
        flush ${fd}
        WriteMarcoLIBS ${fd}

        puts ${fd} "<<< Testing WriteMacroLINKER >>>"
        flush ${fd}
        WriteMacroLINKER ${fd}

        puts ${fd} "<<< Testing WriteMacroOBJFILES >>>"
        flush ${fd}
        WriteMacroOBJFILES ${fd}

        puts ${fd} "<<< Testing WriteRuleALL >>>"
        flush ${fd}
        WriteRuleALL ${fd}

        puts ${fd} "<<< Testing WriteRuleTarget >>>"
        flush ${fd}
        WriteRuleTarget ${fd}

        puts ${fd} "<<< Testing WriteRuleBuilds >>>"
        flush ${fd}
        WriteRuleBuilds ${fd}

        puts ${fd} "<<< Testing WriteRuleDependences >>>"
        flush ${fd}
        WriteRuleDependences ${fd}

    }


    method WriteHeader {file_d} {

        set time [clock format [clock seconds] -format "%r"]
        set date [clock format [clock seconds] -format "%h %d %Y"]

        puts ${file_d}\
          "#########################################################"
        puts ${file_d} "# Makefile auto generated by Cygnus Source Navigator."
        puts ${file_d} "# Target: ${TargetName} Date: ${date} Time: ${time}"
        puts ${file_d} "#\n"
        flush ${file_d}
    }

    method WriteSUFFIXES {file_d} {

        # Make array indexed by valid file extensions
        foreach rule [${toolchain} GetRulesList] {
            if {[${b_target} GetRuleStatus ${rule}]=="Enabled"} {
                foreach rulesuffix [${toolchain} GetRuleSuffixList ${rule}] {
                    set srcsuffix [split ${rulesuffix} "."]

                    # Add the source file extension.
                    set validExtensions(.[lindex ${srcsuffix} 1]) ""

                    # If we can see it, add the target extension (e.g. .o .obj).
                    if {[lindex ${srcsuffix} 2]!=""} {
                        set validExtensions(.[lindex ${srcsuffix} 2]) ""
                    }
                }
            }
        }

        set suffixes ""
        foreach suff [array names validExtensions] {
            set suffixes "${suffixes} ${suff}"
        }
        puts ${file_d} "\n.SUFFIXES:${suffixes}\n"
    }

    method WriteVPATHs {file_d} {
        puts ${file_d} "\nVPATH = [${b_target} GetSourceFilePaths]\n\n"
    }

    method WriteMarcoTool {file_d} {

        # Get a list of tools and types JAVA CC CPP etc...

        set ruletypeslist [${toolchain} GetRulesList]

        foreach rule_type ${ruletypeslist} {
            set tool [${b_target} GetTool ${rule_type}]
            if {${tool}==""} {
                set tool [${toolchain} GetTool ${rule_type}]
            }
            puts ${file_d} "${rule_type} = ${tool}"
        }
    }

    method WriteMarcoFLAGs {file_d} {

        set ruletypeslist [${toolchain} GetRulesList]

        foreach ruletype ${ruletypeslist} {
            set flags ""
            foreach flagstype "Debug Optimize Warning CodeGen" {
                set flagsetting [${b_target} GetToolChainFlags ${ruletype}\
                  ${flagstype}]
                set flags "${flags} [${toolchain} GetRuleFlags ${ruletype}\
                  ${flagstype} ${flagsetting}]"
            }
            set flags "${flags} [${b_target} GetToolChainFlags ${ruletype}\
              UserFlags]"
            set tmp "_FLAGS"
            puts ${file_d} "${ruletype}${tmp} = ${flags}"
        }
    }

    method WriteMarcoINCLUDES {file_d} {
        set ruletypeslist [${toolchain} GetRulesList]

        foreach ruletype ${ruletypeslist} {
            set includes_list1 [${b_target} GetUserIncludes ${ruletype}]
            set includes_list2 [${b_target} GetAutoIncludes ${ruletype}]
            set includes_list [concat ${includes_list1} ${includes_list2}]
            set include_spec [${toolchain} FormatIncludes ${ruletype}\
              ${includes_list}]
            set tmp "_INCLUDES"
            puts ${file_d} "${ruletype}${tmp} = ${include_spec}"
        }
    }

    method WriteMarcoDEFINES {file_d} {
        set ruletypeslist [${toolchain} GetRulesList]

        foreach ruletype ${ruletypeslist} {
            set defines_list [${b_target} GetUserDefines ${ruletype}]
            set defines_spec [${toolchain} FormatDefines ${ruletype}\
              ${defines_list}]
            set tmp "_DEFINES"
            puts ${file_d} "${ruletype}${tmp} = ${defines_spec}"
        }
    }

    method WriteMarcoLIBS {file_d} {
        set lib_files [${b_target} GetLibraryFiles]
        set tmpstr "_LIBS"
        puts ${file_d} "${TargetName}${tmpstr} = ${lib_files}"
    }

    method WriteMacroLINKER {file_d} {
        set linker [${b_target} GetLinkerLocation]
        puts ${file_d} "LINKER = ${linker}"

        set linkerflags [${b_target} GetUserLinkFlags]
        puts ${file_d} "LINKER_FLAGS = ${linkerflags}"

        set entrypoint [${b_target} GetLinkerEntryPoint]
        if {${entrypoint}!=""} {
            set projecttype [${b_target} GuessProjectType]
            set entrypflag [${toolchain} GetLinkerEntryPointFlag ${projecttype}]
            set entrypoint "${entrypflag}${entrypoint}"
        }
        puts ${file_d} "LINKER_ENTRY = ${entrypoint}"
    }

    method WriteMacroOBJFILES {file_d} {

        # Get all of the source files
        set srcfiles [${b_target} GetSourceFiles]

        # Generate object files list.
        set tmpstr "_OBJECTS"
        puts ${file_d} "${TargetName}${tmpstr} =\
          [${toolchain} GetObjectFileList ${srcfiles}]"
    }

    method WriteRuleclean {file_d} {
        global tcl_platform

        # Get all of the source files
        set srcfiles [${b_target} GetSourceFiles]

        # Generate object files list.
        set objfiles [${toolchain} GetObjectFileList ${srcfiles}]

        # Get file extentions
        set objexts ""
        foreach objfile ${objfiles} {
            set objexts "${objexts} [file extension ${objfile}]"
        }

        # make sure they are all unique
        set objexts [lunique [lsort ${objexts}]]

        # Get the executable name
        set outputfile [${b_target} GetOutputFilename]
        if {[${b_target} GetTargetType]=="Executable"} {
            if {$tcl_platform(platform)=="windows"} {
                #TODO: warning, this assumes it's an executable!
                set outputfile "${outputfile}.exe"
            }
        } else {
            # It's must be a library
            set basicflags [${toolchain} GetLibLinkerRule]
            set outputfile "${outputfile}.a"
        }

        # Make the rule
        puts ${file_d} "\nclean:"
        foreach objext ${objexts} {
            puts ${file_d} "\trm -f *${objext}"
        }

        puts ${file_d} "\trm -f ${outputfile}"

        # EL/IX change: for embedded toolchains, also clean the EL/IX
        # config file.
        if {[${toolchain} GetIsEmbedded]} {
            # Don't remove .runcfg, because we might have created it
            # just before running `make clean'.  FIXME.
            puts ${file_d} "\trm -f .olix"
        }

        puts ${file_d} ""
    }

    method WriteRuleALL {file_d} {
        global tcl_platform
        set outputfile [${b_target} GetOutputFilename]
        if {[${b_target} GetTargetType]=="Executable"} {
            if {$tcl_platform(platform)=="windows"} {
                #TODO: warning, this assumes it's an executable!
                set outputfile "${outputfile}.exe"
            }
        } else {
            # It's must be a library
            set basicflags [${toolchain} GetLibLinkerRule]
            set outputfile "${outputfile}.a"
        }

	# If there is a post linker stage we'll need to
	# the post linker suffix
	if {[$toolchain GetPostLinkerSuffixes] != ""} {
	    set outputfile $outputfile[$toolchain GetPostLinkerSuffixes]
	}
        puts ${file_d} "\nall: ${outputfile}\n"
    }

    method WriteRuleTarget {file_d} {
        global tcl_platform sn_options sn_elix

        set outputfile [${b_target} GetOutputFilename]
        if {[${b_target} GetTargetType]=="Executable"} {
            if {$tcl_platform(platform)=="windows"} {
                #TODO: warning, this assumes it's an executable!
                set outputfile "${outputfile}.exe"
            }
            set basicflags [${toolchain} GetExeLinkerRule]
        } else {
            # It's must be a library
            set basicflags [${toolchain} GetLibLinkerRule]
            set outputfile "${outputfile}.a"
        }

        set tmpstr2 "_LIBS"
        set tmpstr "_OBJECTS"
        # EL/IX change: for embedded targets, depend on runcfg file.
        # This is somewhat broken because it means we'll relink every
        # time `make' is run.  Something more graceful would be nice.
        # We also arrange for elix-link to be used.
        if {${sn_elix} && [${toolchain} GetIsEmbedded]} {
            global sn_options

            set prefix [file join [pwd] [${b_target} GetBuildDirectory]]/
            set fd [open ${prefix}.runcfg w]
            set cmd [Elix&::make_config_command [${toolchain} GetShortName]\
              ${prefix}]
            puts ${fd} ${cmd}
            close ${fd}

            set mode ""
            if {[${b_target} GetLaunchWithDebugger]} {
                set mode "--mode debug"
            }

            puts ${file_d} "${outputfile}: \$(${TargetName}${tmpstr})\
              ${prefix}.runcfg"
            puts ${file_d} "\telix-link --prefix ${prefix}\
              --output ${outputfile} ${mode} -- $(LINKER) $(LINKER_ENTRY)\
              $(LINKER_FLAGS) $(${TargetName}${tmpstr})\
              $(${TargetName}${tmpstr2})\n"
        } else {
            puts ${file_d} "${outputfile}: \$(${TargetName}${tmpstr})"
            puts ${file_d} "\t\$(LINKER) ${basicflags} ${outputfile}\
              \$(LINKER_ENTRY) \$(LINKER_FLAGS) \$(${TargetName}${tmpstr})\
              \$(${TargetName}${tmpstr2})\n"
        }
    }

    method WriteRuleBuilds {file_d} {
        set ruletypeslist [${toolchain} GetRulesList]

        foreach ruletype ${ruletypeslist} {

            # If the rule is disabled, move on to the next rule.
            if {[${b_target} GetRuleStatus ${ruletype}]=="Disabled"} {
                continue
            }

            set rulesuffixlist [${toolchain} GetRuleSuffixList ${ruletype}]
            foreach rulesuffix ${rulesuffixlist} {
                puts ${file_d} "${rulesuffix}:"
                set basicaction [${toolchain} GetBasicAction ${ruletype}]
                set tmpstr1 "_FLAGS"
                set tmpstr2 "_DEFINES"
                set tmpstr3 "_INCLUDES"
                puts ${file_d} "\t\$(${ruletype}) ${basicaction}\
                  \$(${ruletype}${tmpstr1}) \$(${ruletype}${tmpstr2})\
                  \$(${ruletype}${tmpstr3})"
            }
            puts ${file_d} "\n"
        }
    }

    method WritePostLinkRule {file_d} {
	if {[$toolchain GetUserPostLinkerRuleProc] == ""} {
	    # There is no post link build step.
	    return
	}

	# Called the custom post linker rule generator proc
	# with build target and toolchain.  Write the
	# results to the Makefile.

	set postlinkruleproc [$toolchain GetUserPostLinkerRuleProc]

	# TODO: Calls to User fuctions need a  catch {...}
	set postlinkrule [$postlinkruleproc \
		$toolchain [itcl::code $b_target] [${b_target} GetOutputFilename]]
	if {$postlinkrule != ""} {
	    puts $file_d "\n"
	    puts $file_d $postlinkrule
	    puts $file_d "\n"
	}
    }


    method WriteRuleDependences {file_d} {

        # Check paf_db_iu exists
        if {[::info command paf_db_iu] == ""} {
            # Sorry, we need the includes database to generate dependences
            return
        }

        # Get all of the source files and their paths
        set srcfiles [${b_target} GetSourceFiles]
        set srcpaths [${b_target} GetSourceFilePaths]

        foreach srcfile ${srcfiles} {

            # Get include dependences
            set includefiles [paf_db_iu seq -col 0 -end ${srcfile}]

            # Remove any we can't find (system includes,etc)
            set existing_includes ""
            foreach includefile ${includefiles} {
                if {[file exists ${includefile}]} {
                    set path_relative_name ${includefile}

                    if {[file pathtype ${path_relative_name}] != "absolute"} {
                        set path_relative_name [file join\
                          [pwd] ${path_relative_name}]

                        # Check to see if one of the prefixes on the path\
                          matches
                        # the absolute pathname of this file. If this is the\
                          case,
                        # we want to trim off the matching bits off our project
                        # relative pathname.
                        #
                        # So if our path is { /mysrc /a/b }, our list of files
                        # is { b/b.txt c/c.txt }, and our current directory is\
                          { /a },
                        # then the resulting files would be { b.txt /a/c/c.txt }

                        foreach path ${srcpaths} {
                            set len [string length ${path}]
                            if {${len} == 0} {
                                continue
                            }
                            # If the path does not end with a / char, add one
                            if {[string index ${path} [expr {${len} - 1}]] !=\
                              "/"} {
                                append path /
                                set len [string length ${path}]
                            }
                            if {[string compare [string range\
                              ${path_relative_name} 0 [expr {${len} - 1}]]\
                              ${path}] == 0} {
                                # Trim off the matching path prefix to get the\
                                  path relative filename
                                set path_relative_name [string range\
                                  ${path_relative_name} ${len} end]
                                break
                            }
                        }
                    }

                    lappend existing_includes\
                      [despace_pathname ${path_relative_name}]
                }
            }
            if {${existing_includes} != {}} {
                # Get object file name.
                set objectfile [${toolchain} GetObjectFileList ${srcfile}]

                puts ${file_d} "${objectfile}: [join ${existing_includes} " "]"
            }
        }
    }

}


proc despace_pathname {ppath} {
    global tcl_platform

    if {$tcl_platform(platform)=="windows"} {
        set path_so_far ""
        foreach path [file split ${ppath}] {
            if {[llength ${path}]!=1} {
                # Path item has a space in it.
                # Get the -short pathname of the element with the
                # space in it, being careful not to -short anything else.
                set path [lindex [file attributes [file join ${path_so_far}\
                  ${path}] -short] end]
            }

            # Stick it back together.
            set path_so_far [file join ${path_so_far} ${path}]
        }
    } else {
        set path_so_far ${ppath}
    }
    return ${path_so_far}
}

