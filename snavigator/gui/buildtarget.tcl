# Copyright (c) 2000, Red Hat, Inc.
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
## Build Target backend data handling for SN-IDE.
## Copyright (c) 1999 Cygnus Solutions, Inc.
## Written by Ian Roxborough <irox@cygnus.com>
##
################################################

itcl::class snBuildTarget {


    public variable TargetType "Executable"
    public variable TargetName ""
    public variable ToolChain ""

    protected variable IncludesAuto
    # Array of auto gen includes for a tool.
    protected variable IncludesUser
    # Array of user includes for a tool.
    protected variable Defines
    # Array of defines for a tool.
    protected variable ToolChainFlags
    # Array of selected flags for a tool.
    protected variable ruleStatus
    # Array of rule's status.
    protected variable SrcFiles ""
    protected variable LibFiles ""
    protected variable BuildDir ""
    protected variable OutputFilename ""
    protected variable UserLinkFlags ""
    protected variable LinkerEntryPoint ""
    protected variable LinkerApplication ""
    # Which linker to use.
    protected variable LaunchDebugger 0
    # if 1 launch with debugger.
    protected variable LaunchCommandLine ""
    # Run/debug command line.
    protected variable NeedsRebuild 0
    # if 1 we need a `clean' first.

    # User custom client data.
    public variable userToolChainData ""

    method SetUserToolChainData {data} {
	set userToolChainData $data
    }

    method GetUserToolChainData {} {
	return $userToolChainData
    }

    ######################################
    # snBuildTarget::SetBuildDirectory
    #
    # Sets the build directory to store intermidiate and final files.
    #

    method SetBuildDirectory {build_dir} {
        set BuildDir ${build_dir}
    }

    method GetBuildDirectory {} {
        return ${BuildDir}
    }

    method SetTargetType {target_type} {
        set TargetType ${target_type}
    }

    method GetTargetType {} {
        return ${TargetType}
    }

    method SetNeedsRebuild {val} {
        set NeedsRebuild ${val}
    }

    method GetNeedsRebuild {} {
        return ${NeedsRebuild}
    }

    method SetToolChain {tool_chain} {
        set ToolChain ${tool_chain}
        set NeedsRebuild 1
    }

    method GetToolChain {} {
        return ${ToolChain}
    }

    method SetRuleStatus {rule_type rule_status} {
        set ruleStatus(${rule_type}) ${rule_status}
    }

    method GetRuleStatus {rule_type} {
        if {![info exists ruleStatus(${rule_type})]} {
            return "Enabled"
        }
        if {$ruleStatus(${rule_type})==""} {
            return "Enabled"
        }
        return $ruleStatus(${rule_type})
    }

    ######################################
    # sn_BuildTarget::AddSourceFiles
    #
    # Add sources file to current build target

    method AddSourceFiles {new_files} {

        set new_files [FilterSourceFiles ${new_files}]

        set SrcFiles [concat ${SrcFiles} ${new_files}]

        set SrcFiles [lsort $SrcFiles]
        set SrcFiles [lunique $SrcFiles]
    }

    method FilterSourceFiles {{files ""}} {

        if {${files}==""} {
            return
        }

        set validFileList ""

        # Make array indexed by valid file extensions
        set tchain_obj [GetToolChainObject ${ToolChain}]
        foreach rule [${tchain_obj} GetRulesList] {
            foreach rulesuffix [${tchain_obj} GetRuleSuffixList ${rule}] {
                set srcsuffix [split ${rulesuffix} "."]
                set validExtensions(.[lindex ${srcsuffix} 1]) ""
            }
        }

        foreach file ${files} {

            # Get File extension
            set ext [file extension ${file}]

            if {[info exists validExtensions(${ext})]} {
                lappend validFileList ${file}
            }

        }

        return ${validFileList}
    }

    method SetSourceFiles {{src_files ""}} {

        set SrcFiles ""
        if {${src_files}!=""} {
            AddSourceFiles ${src_files}
        }
    }

    method RemoveSourceFiles {dead_files} {
        sn_log "IDE_DEBUG(snBuildTarget::RemoveSourceFiles) removing\
          ${dead_files}"
        foreach file ${dead_files} {
            set deadone [lsearch ${SrcFiles} ${file}]
            set SrcFiles [lreplace ${SrcFiles} ${deadone} ${deadone}]
        }
        sn_log "IDE_DEBUG(snBuildTarget::RemoveSourceFiles) files = ${SrcFiles}"
    }

    method GetToolChainFlags {rule_type flags_type} {

        if {[info exists ToolChainFlags(${rule_type}${flags_type})]==1} {
            return $ToolChainFlags(${rule_type}${flags_type})
        } else {
            return ""
        }
    }


    method GetTool {rule_type} {

        set tool [GetToolChainFlags ${rule_type} ToolLocation]
        if {${tool}==""} {
            set tchain_obj [GetToolChainObject ${ToolChain}]
            set tool [${tchain_obj} GetTool ${rule_type}]
        }
        return ${tool}

    }

    method SetToolChainFlags {rule_type flags_type option} {
        set ToolChainFlags(${rule_type}${flags_type}) ${option}
    }

    method SetUserIncludes {rule_type include_paths} {
        #TODO: check rule_type is good.
        set IncludesUser(${rule_type}) ${include_paths}
    }

    method SetAutoIncludes {rule_type include_paths} {
        #TODO: check rule type is good.
        set IncludesAuto(${rule_type}) ${include_paths}
    }

    method GetUserIncludes {rule_type} {
        #TODO: check rule type is good.
        if {[info exists IncludesUser(${rule_type})]} {
            return $IncludesUser(${rule_type})
        } else {
            return ""
        }
    }

    method GetAutoIncludes {rule_type} {
        #TODO: check rule type is good.
        if {[info exists IncludesAuto(${rule_type})]} {
            return $IncludesAuto(${rule_type})
        } else {
            return ""
        }
    }

    method SetUserDefines {rule_type defines} {
        #TODO: check rule type is good.
        set Defines(${rule_type}) ${defines}
    }

    method GetUserDefines {rule_type} {
        #TODO: check rule type is good.
        if {[info exists Defines(${rule_type})]} {
            return $Defines(${rule_type})
        } else {
            return ""
        }
    }

    method SetLibraryFiles {{lib_files ""}} {
        set LibFiles ${lib_files}
    }

    method GetLibraryFiles {} {
        return ${LibFiles}
    }

    method CreateNewTarget {} {
        #TODO: inialize lots of data
        #TODO: work out which data to inialize.
    }

    method GetSourceFiles {} {
        return ${SrcFiles}
    }

    method GetSourceFilePaths {} {
        set srcfiles [GetSourceFiles]

        #get source file path names
        foreach srcfile ${srcfiles} {
            set dirname [file dirname ${srcfile}]
            if {[file pathtype ${srcfile}] != "absolute"} {
                set dirname [file join [pwd] ${dirname}]
            }
            set filterdups([despace_pathname ${dirname}]) ""
        }

        return [join [array names filterdups] " "]
    }

    method GetDebugger {} {
        global sn_options
        set tchain_obj [GetToolChainObject ${ToolChain}]
        set debug [${tchain_obj} GetDebuggerName]
        if {${debug} == ""} {
            set debug $sn_options(def,gdb-command)
        }
        return ${debug}
    }

    method GetLaunchCommandLine {} {
        if {${LaunchCommandLine}==""} {
            # We want this to work even if `.' is not in path, so we
            # add the `./'.
            return [file join . [GetOutputFilename]]
        } else {
            return ${LaunchCommandLine}
        }
    }

    method SetLaunchCommandLine {cmd_line} {
        if {${cmd_line}==[GetOutputFilename]} {
            set LaunchCommandLine ""
        } else {
            set LaunchCommandLine ${cmd_line}
        }
    }

    method GetOutputFilename {} {
        if {${OutputFilename}!=""} {
            return ${OutputFilename}
        } else {
            return ${TargetName}
        }
    }

    method SetOutputFilename {output_filename} {
        set OutputFilename ${output_filename}
    }

    method SetUserLinkFlags {flags} {
        set UserLinkFlags ${flags}
    }

    method GetUserLinkFlags {} {
        return ${UserLinkFlags}
    }

    method SetLinkerEntryPoint {entry_point} {
        set LinkerEntryPoint ${entry_point}
    }

    method GetLinkerEntryPoint {} {
        return ${LinkerEntryPoint}
    }

    method SetLinkerLocation {linker} {

        set projecttype [GuessProjectType]
        set tool [GetTool ${projecttype}]
        if {${tool}==""} {
            set tchain_obj [GetToolChainObject ${ToolChain}]
            if {${TargetType}=="Executable"} {
                set tool [GetTool ${projecttype}]
            } else {
                #Must be a library then.
                set tool [${tchain_obj} GetLibLinkerTool]
            }
        }
        if {${linker}==${tool}} {
            # We don't need to set it since we know
            # we can guess it.
            set LinkerApplication ""
        } else {
            set LinkerApplication ${linker}
        }
    }

    method GuessProjectType {} {

        set tchain_obj [GetToolChainObject ${ToolChain}]

        set JavaFiles 0
        set CPPFiles 0
        set CFiles 0

        foreach file ${SrcFiles} {
            switch [${tchain_obj} GetFileType ${file}] {
                JAVA {
                        incr JavaFiles
                    }
                CPP {
                        incr CPPFiles
                    }
                CC {
                        incr CFiles
                    }
            }
        }

        if {${JavaFiles}!=0} {
            return JAVA
        }
        if {${CPPFiles}!=0} {
            return CPP
        }
        # NB: The file type might be UNKNOWN but return CC anyway.
        return CC
    }

    method GetLinkerLocation {} {
        set tchain_obj [GetToolChainObject ${ToolChain}]
        if {${LinkerApplication}==""} {
            set projecttype [GuessProjectType]
            if {${TargetType}=="Executable"} {
                set tool [GetTool ${projecttype}]
            } else {
                #Must be a library then.
                set tool [${tchain_obj} GetLibLinkerTool]
            }

            if {${tool}==""} {
                set tchain_obj [GetToolChainObject ${ToolChain}]
                set tool [GetTool ${projecttype}]
            }

            return ${tool}
        }

        return ${LinkerApplication}
    }

    method SetLaunchWithDebugger {use_debugger} {
        set LaunchDebugger ${use_debugger}
    }

    method GetLaunchWithDebugger {} {
        if {${LaunchDebugger}==""} {
            set LaunchDebugger 0
        }

        return ${LaunchDebugger}
    }

    method LoadData {{target_name ""}} {
        sn_log "IDE_DEBUG(snBuildTarget::LoadData)"

        if {${target_name}!=""} {
            set TargetName ${target_name}
        }

	# Get user custom/client data.
	set userToolChainData [paf_db_proj get -key "IDEUserData${TargetName}"]

        # Get Build Directory
        set BuildDir [paf_db_proj get -key "IDEBuildDir${TargetName}"]

        # Get Target Type
        set db_target_type [paf_db_proj get -key "IDETargetType${TargetName}"]
        if {${db_target_type}!=""} {
            set TargetType ${db_target_type}
        }

        # Get Tool Chain
        set ToolChain [paf_db_proj get -key "IDEToolChain${TargetName}"]

        # Get Source Files
        set SrcFiles [paf_db_proj get -key "IDESourceFiles${TargetName}"]

        # Get the link output file name.
        set OutputFilename [paf_db_proj get -key "IDELinkOutFile${TargetName}"]

        # Get the user linker flags.
        set UserLinkFlags [paf_db_proj get\
          -key "IDEUserLinkerFlags${TargetName}"]

        # Get the link entry point.
        set LinkerEntryPoint [paf_db_proj get\
          -key "IDELinkEntryPoint${TargetName}"]

  	# Get the link command/executable
        set LinkerApplication [paf_db_proj get\
	  -key "IDELinkerApplication${TargetName}"]

        # Get the defines list
        # TODO: Get rule type from toolchain
        foreach ruletype "CC CPP JAVA" {
            set Defines(${ruletype}) [paf_db_proj get\
              -key "IDEMacroDefines${ruletype}${TargetName}"]
        }

        # Get Auto Generated Includes.
        set IncludesAuto(CC) [paf_db_proj get\
          -key "IDEGenIncludesCC${TargetName}"]
        set IncludesAuto(CPP) [paf_db_proj get\
          -key "IDEGenIncludesCPP${TargetName}"]
        set IncludesAuto(JAVA) [paf_db_proj get\
          -key "IDEGenIncludesJAVA${TargetName}"]

        # Get User specified Includes.
        set IncludesUser(CC) [paf_db_proj get\
          -key "IDEUserIncludesCC${TargetName}"]
        set IncludesUser(CPP) [paf_db_proj get\
          -key "IDEUserIncludesCPP${TargetName}"]
        set IncludesUser(JAVA) [paf_db_proj get\
          -key "IDEUserIncludesJAVA${TargetName}"]

        # Get Launch with debugger flag.
        set LaunchDebugger [paf_db_proj get\
          -key "IDEUseDebuggerOnLaunch${TargetName}"]
        set LaunchCommandLine [paf_db_proj get\
          -key "IDELaunchCommandLine${TargetName}"]

        set NeedsRebuild [paf_db_proj get -key "IDENeedsRebuild${TargetName}"]
        if {${NeedsRebuild} == ""} {
            set NeedsRebuild 0
        }

        LoadToolChainFlags

        # Load Rule Status settings.
        LoadRuleStatus

        # Get Librarys to linked against.
        set LibFiles [paf_db_proj get -key "IDELibFiles${TargetName}"]

        paf_db_proj sync
    }

    method SaveData {{target_name ""}} {
        sn_log "IDE_DEBUG(snBuildTarget::SaveData)"

        if {${target_name}!=""} {
            set TargetName ${target_name}
        }

	# Store user custom/client data.
	paf_db_proj put "IDEUserData${TargetName}" ${userToolChainData}

        # Store Build Directory
        paf_db_proj put "IDEBuildDir${TargetName}" ${BuildDir}

        # Store Target Type
        paf_db_proj put "IDETargetType${TargetName}" ${TargetType}

        # Store Tool Chain
        paf_db_proj put "IDEToolChain${TargetName}" ${ToolChain}

        # Store Source Files
        paf_db_proj put "IDESourceFiles${TargetName}" ${SrcFiles}

        # Store the link output file name.
        paf_db_proj put "IDELinkOutFile${TargetName}" ${OutputFilename}

        # Store the user linker flags.
        paf_db_proj put "IDEUserLinkerFlags${TargetName}" ${UserLinkFlags}

        # Store the link entry point.
        paf_db_proj put "IDELinkEntryPoint${TargetName}" ${LinkerEntryPoint}

 	# Store the link command/executable
        paf_db_proj put "IDELinkerApplication${TargetName}" $LinkerApplication

        # Store the defines list
        # TODO: Get rule type from toolchain
        foreach ruletype "CC CPP JAVA" {
            paf_db_proj put\
              "IDEMacroDefines${ruletype}${TargetName}" $Defines(${ruletype})
        }

        # Store Auto Generated Includes.
        paf_db_proj put "IDEGenIncludesCC${TargetName}" $IncludesAuto(CC)
        paf_db_proj put "IDEGenIncludesCPP${TargetName}" $IncludesAuto(CPP)
        paf_db_proj put "IDEGenIncludesJAVA${TargetName}" $IncludesAuto(JAVA)

        # Store User specified Includes.
        paf_db_proj put "IDEUserIncludesCC${TargetName}" $IncludesUser(CC)
        paf_db_proj put "IDEUserIncludesCPP${TargetName}" $IncludesUser(CPP)
        paf_db_proj put "IDEUserIncludesJAVA${TargetName}" $IncludesUser(JAVA)

        # Save ToolChain flags.
        SaveToolChainFlags

        # Save Rule Status settings.
        SaveRuleStatus

        # Store Launch with debugger flag.
        paf_db_proj put "IDEUseDebuggerOnLaunch${TargetName}" ${LaunchDebugger}
        paf_db_proj put "IDELaunchCommandLine${TargetName}" ${LaunchCommandLine}

        paf_db_proj put "IDENeedsRebuild${TargetName}" ${NeedsRebuild}

        # Store Librarys to linked against.
        paf_db_proj put "IDELibFiles${TargetName}" ${LibFiles}

        paf_db_proj sync
    }

    method DeleteData {{target_name ""}} {
        sn_log "IDE_DEBUG(snBuildTarget::DeleteData)"

        if {${target_name}!=""} {
            set TargetName ${target_name}
        }

	# Delete user custom/client data.
	paf_db_proj delete "IDEUserData${TargetName}"

        # Delete Build Directory
        paf_db_proj delete "IDEBuildDir${TargetName}"

        # Delete Target Type
        paf_db_proj delete "IDETargetType${TargetName}"

        # Delete Tool Chain
        paf_db_proj delete "IDEToolChain${TargetName}"

        # delete Source Files
        paf_db_proj delete "IDESourceFiles${TargetName}"

        # delete the link output file name.
        paf_db_proj delete "IDELinkOutFile${TargetName}"

        # delete the user linker flags.
        paf_db_proj delete "IDEUserLinkerFlags${TargetName}"

        # delete the link entry point.
        paf_db_proj delete "IDELinkEntryPoint${TargetName}"

 	# delete the link command/executable
        paf_db_proj delete "IDELinkerApplication${TargetName}"

        # delete the defines list
        # TODO: Get rule type from toolchain
        foreach ruletype "CC CPP JAVA" {
            paf_db_proj delete "IDEMacroDefines${ruletype}${TargetName}"
        }

        # delete Auto Generated Includes.
        paf_db_proj delete "IDEGenIncludesCC${TargetName}"
        paf_db_proj delete "IDEGenIncludesCPP${TargetName}"
        paf_db_proj delete "IDEGenIncludesJAVA${TargetName}"

        # delete User specified Includes.
        paf_db_proj delete "IDEUserIncludesCC${TargetName}"
        paf_db_proj delete "IDEUserIncludesCPP${TargetName}"
        paf_db_proj delete "IDEUserIncludesJAVA${TargetName}"

        DeleteToolChainFlags

        # Delete Rule Status settings.
        DeleteRuleStatus

        # Delete Launch with debugger flag.
        paf_db_proj delete "IDEUseDebuggerOnLaunch${TargetName}"
        paf_db_proj delete "IDELaunchCommandLine${TargetName}"

        paf_db_proj delete "IDENeedsRebuild${TargetName}"

        # delete Librarys to linked against.
        paf_db_proj delete "IDELibFiles${TargetName}"

        paf_db_proj sync
    }

    method SaveToolChainFlags {} {
        set flagkeys [array names ToolChainFlags]

        paf_db_proj put "IDEFlagKeys${TargetName}" ${flagkeys}

        foreach flagkey ${flagkeys} {
            paf_db_proj put\
              "IDEFlagKey_${flagkey}${TargetName}" $ToolChainFlags(${flagkey})
        }
    }

    method LoadToolChainFlags {} {
        set flagkeys [paf_db_proj get -key "IDEFlagKeys${TargetName}"]

        foreach flagkey ${flagkeys} {
            set ToolChainFlags(${flagkey}) [paf_db_proj get\
              -key "IDEFlagKey_${flagkey}${TargetName}"]
        }
    }

    method DeleteToolChainFlags {} {
        set flagkeys [paf_db_proj get -key "IDEFlagKeys${TargetName}"]

        paf_db_proj delete "IDEFlagKeys${TargetName}"

        foreach flagkey ${flagkeys} {
            paf_db_proj delete "IDEFlagKey_${flagkey}${TargetName}"
        }
    }

    method SaveRuleStatus {} {
        set rulestatuskeys [array names ruleStatus]

        # Only Save the disabled rules.
        set disabledRules ""
        foreach rulestat ${rulestatuskeys} {
            if {[GetRuleStatus ${rulestat}]=="Disabled"} {
                set disabledRules "${disabledRules} ${rulestat}"
            }
        }
        paf_db_proj put "IDEDisabledRules${TargetName}" ${disabledRules}
    }

    method LoadRuleStatus {} {
        set rulestatuskeys [paf_db_proj get\
          -key "IDEDisabledRules${TargetName}"]

        foreach rulestat ${rulestatuskeys} {
            set ruleStatus(${rulestat}) "Disabled"
        }
    }

    method DeleteRuleStatus {} {
        paf_db_proj delete "IDEDisabledRules${TargetName}"
    }

}


