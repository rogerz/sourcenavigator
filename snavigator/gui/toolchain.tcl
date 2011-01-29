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
## Toolchain spec and class for SN-IDE.
## Copyright (c) 1999 Cygnus Solutions, Inc.
## Written by Ian Roxborough <irox@cygnus.com>
##
################################################

itcl_class snToolChain {

    protected ruleFlagsIDTagsList
    # array of IDTags for a flag_type for a rule
    protected rules
    # array of basic actions a rule
    protected ruleFlagsDebug
    # array of Debug flags
    protected ruleFlagsOptimize
    # array of Optimization flags
    protected ruleFlagsWarning
    # array of Warning flags
    protected ruleFlagsCodeGen
    # array of Code Generation flags
    protected ruleSuffix
    # array of rule suffixes
    protected ruleTool
    # array of "locations" of tools
    protected FormatDefinesProcs
    # array of define format proc of a tools
    protected FormatIncludeProcs
    # array of define format proc of a tools
    protected FileInfo
    # array of file type used by rules
    protected RuleInfo
    # array of general info for each rule
    protected linkerTool
    # Linker name/location.
    protected applicationEntryPoint ;# Flags for the entry point.
    protected isEmbedded
    # True if embedded (EL/IX) toolchain
    protected debuggerName
    # Name of debugger for target
    protected shortName
    # Short name of target.

    # Long name of toolchain i.e. "GNUPro elix x86"
    protected fulldescription     ""

    # The tool (if any) to be used after the link stage.
    protected postLinkerTool      ""
    protected postLinkerSuffixes  ""
    protected postLinkerFlags     ""

    # The proc to generate the rule for post Linking.
    protected userPostLinkerRule  ""

    # The proc for creating and destorying custom target edit GUI.
    protected userGUIcode         ""
    protected userRemoveGUIcode   ""


    ##################################################################
    # snToolChain::AddRule
    # Add the basic rule/flags required to compile/transform/etc
    #
    # Parameters:
    #       rule_type: CC CPP JAVA OTHER
    #       rule_suffix: .c.obj .c.o .java.o etc
    #       bin_tool: the binary to be executed, gcc, cl.exe, /usr/bin/gcc, etc
    #       basic_action: "-c $<" minimal required flags

    method AddRule {rule_type rule_suffix bin_tool basic_action} {

        set ruleSuffix(${rule_type}) ${rule_suffix}
        set rules(${rule_type}) ${basic_action}
        set ruleTool(${rule_type}) ${bin_tool}

    }

    method AddRuleSuffix {rule_type rule_suffix} {
        set ruleSuffix(${rule_type}) [concat $ruleSuffix(${rule_type})\
          ${rule_suffix}]
    }

    ##################################################################
    # snToolChain::AddTool
    # Add a tool assocated with a rule type, i.e. CC -> gcc.
    #
    # Parameters:
    #       rule_type: CC CPP JAVA OTHER
    #       tool: the binary to be executed, gcc, cl.exe, /usr/bin/gcc, etc

    method AddTool {rule_type tool} {
        set ruleSuffix(${rule_type}) ${tool}
    }


    ##################################################################
    # snToolChain::GetTool
    # Returns the tool given the rule_type.
    #
    # Parameters:
    #       rule_type: CC CPP JAVA ...
    #
    # returns the the binary to be executed, gcc, cl.exe, /usr/bin/gcc, etc

    method GetTool {rule_type} {
        return $ruleTool(${rule_type})
    }


    ##################################################################
    # snToolChain::AddFlagSet
    # Add Optimization flags to a rule.
    # Used to add flag(s) set to the rule.  Each set of flags
    # will be selectable in the rule settings dialog later.
    #
    # Parameters:
    #      rule_suffix - the rule that the flags apply to (e.g. .c.o)
    #      flagType - Debug, Optimize, Warning  
    #      flags - the flags to be used (e.g. -Os -O2)
    #      info - this is the information to displayed in the
    #      widget used to select the flags set.  This information
    #      maybe a text string or one of the predefined flag sets:
    #          OPT_NONE - None.
    #          OPT_STD - Standard.
    #          OPT_SPEED - Maximum Speed.
    #          OPT_SIZE - Minimum Size.
    #
    #          DBG_NONE - None.
    #          DBG_STD - Standard debug.

    method AddRuleFlagSet {rule_type flag_type flags id_tag} {

        #TODO: check rule exists
        #TODO: check valid flag type
        switch ${flag_type} {
            Debug {
                    set ruleFlagsDebug(${rule_type}${id_tag}) ${flags}
                }
            Optimize {
                    set ruleFlagsOptimize(${rule_type}${id_tag}) ${flags}
                }
            Warning {
                    set ruleFlagsWarning(${rule_type}${id_tag}) ${flags}
                }
            CodeGen {
                    set ruleFlagsCodeGen(${rule_type}${id_tag}) ${flags}
                }
        }

        lappend ruleFlagsIDTagsList(${rule_type}${flag_type}) ${id_tag}
    }

    method AddRuleInfo {rule file_type general_description} {
        #TODO: check rule is valid
        set FileInfo(${rule}) ${file_type}
        set RuleInfo(${rule}) ${general_description}
    }

    method GetFileInfo {rule} {
        return [set FileInfo(${rule})]
    }

    method GetGeneralInfo {rule} {
        return [set RuleInfo(${rule})]
    }

    method GetRulesList {} {
        return [array names ruleTool]
    }

    method GetRuleSuffixList {rule_type} {
        return $ruleSuffix(${rule_type})
    }

    method GetBasicAction {rule_type} {
        return $rules(${rule_type})
    }

    method GetRuleFlags {rule_type flag_type id_tag} {

        #TODO: check rule exists
        #TODO: check valid flag type
        set flags ""

        switch ${flag_type} {
            Debug {
                    if {[info exists ruleFlagsDebug(${rule_type}${id_tag})]} {
                        set flags $ruleFlagsDebug(${rule_type}${id_tag})
                    }
                }
            Optimize {
                    if {[info exists\
                      ruleFlagsOptimize(${rule_type}${id_tag})]} {
                        set flags $ruleFlagsOptimize(${rule_type}${id_tag})
                    }
                }
            Warning {
                    if {[info exists ruleFlagsWarning(${rule_type}${id_tag})]} {
                        set flags $ruleFlagsWarning(${rule_type}${id_tag})
                    }
                }
            CodeGen {
                    if {[info exists ruleFlagsCodeGen(${rule_type}${id_tag})]} {
                        set flags $ruleFlagsCodeGen(${rule_type}${id_tag})
                    }
                }
        }
        return ${flags}
    }

    method GetRuleFlagsIDTagsList {rule_type flag_type} {
        return $ruleFlagsIDTagsList(${rule_type}${flag_type})
    }

    method AddExeLinkerEntryPointFlag {rule_type flags} {
        set applicationEntryPoint(${rule_type}) ${flags}
    }

    method GetLinkerEntryPointFlag {rule_type} {
        if {[info exists applicationEntryPoint(${rule_type})]} {
            return $applicationEntryPoint(${rule_type})
        } else {
            return ""
        }
    }

    method AddExeLinkerTool {tool} {
        set linkerTool(exeTool) ${tool}
    }

    method GetExeLinkerTool {} {
        if {[info exists linkerTool(exeTool)]} {
            return $linkerTool(exeTool)
        }
        return ""
    }

    method AddExeLinkerRule {basic_action} {
        set linkerTool(exeAction) ${basic_action}
    }

    method GetExeLinkerRule {} {
        if {[info exists linkerTool(exeAction)]} {
            return $linkerTool(exeAction)
        }
        return ""
    }

    method AddExeLinkerFlagSet {flag_type flags info} {
    }

    method AddLibLinkerTool {tool} {
        set linkerTool(libTool) ${tool}
    }

    method GetLibLinkerTool {} {
        if {[info exists linkerTool(libTool)]} {
            return $linkerTool(libTool)
        }
        return ""
    }

    method AddLibLinkerRule {basic_action} {
        set linkerTool(libAction) ${basic_action}
    }

    method GetLibLinkerRule {} {
        if {[info exists linkerTool(libAction)]} {
            return $linkerTool(libAction)
        }
        return ""
    }

    method SetUserTargetEditGUI {proc} {
	set userGUIcode $proc
    }

    # This is called to add custom GUI
    # bit to the targeteditor dialog.
    method GetUserTargetEditGUI {} {
	return $userGUIcode
    }

    method SetRemoveUserTargetEditGUI {proc} {
	set userRemoveGUIcode $proc
    }

    # This is called to add custom GUI
    # bit to the targeteditor dialog.
    method GetRemoveUserTargetEditGUI {} {
	return $userRemoveGUIcode
    }

    # The proc set here will be used to
    # generate the make rule later.
    method SetUserPostLinkerRuleProc {proc suffixes} {
	set userPostLinkerRule $proc
	set postLinkerSuffixes $suffixes
    }

    method GetUserPostLinkerRuleProc {} {
	return $userPostLinkerRule
    }

    method GetPostLinkerSuffixes {} {
	return $postLinkerSuffixes
    }

    method SetLibSpecProc {spec_proc} {
        set GenLibSpec ${specProc}
    }

    method SetIncludeSpecProc {rule_type spec_proc} {
        #TODO: Check rule type is good
        set FormatIncludeProcs(${rule_type}) ${spec_proc}
    }

    method SetDefinesSpecProc {rule_type spec_proc} {
        #TODO: Check rule type is good
        set FormatDefinesProcs(${rule_type}) ${spec_proc}
    }

    method FormatIncludes {rule_type includes_list} {
        #TODO: Check rule type is good
        return [$FormatIncludeProcs(${rule_type}) ${includes_list}]
    }

    method FormatDefines {rule_type defines_list} {
        #TODO: Check rule type is good
        return [$FormatDefinesProcs(${rule_type}) ${defines_list}]
    }

    method GetFileType {src_file} {

        # Get it's extention
        set fileext [lindex [split [file extension ${src_file}] .] 1]

        foreach ruletype [GetRulesList] {
            foreach rulesuffix $ruleSuffix(${ruletype}) {
                set suffix [lindex [split ${rulesuffix} "."] 1]
                if {${fileext}==${suffix}} {
                    return ${ruletype}
                }
            }
        }
        return UNKNOWN
    }

    method GetObjectFileList {src_file_list} {
        # Get list of rule types
        set ruletypes [array names ruleSuffix]

        # Create an array for file translation look ups
        foreach ruletype ${ruletypes} {
            foreach suffixset $ruleSuffix(${ruletype}) {
                set suffix [split ${suffixset} "."]
                set suffixlookup([lindex ${suffix} 1]) [lindex ${suffix} 2]
            }
        }

        set objfiles ""
        # Process src_file_list to get object file list
        foreach srcfile ${src_file_list} {
            set fileext [lindex [split [file extension ${srcfile}] .] 1]
            while {[info exists suffixlookup(${fileext})]} {
                set fileext $suffixlookup(${fileext})
            }

            # if the file extention (fileent) is set to nothing
            # then this is probably a glich or bug, ignore it.
            if {${fileext}!=""} {
                lappend objfiles "[file tail [file rootname\
                  ${srcfile}]].${fileext}"
            }
        }
        return ${objfiles}
    }

    method GetIsEmbedded {} {
        return ${isEmbedded}
    }

    method SetIsEmbedded {val} {
        set isEmbedded ${val}
    }

    method SetDebuggerName {name} {
        set debuggerName ${name}
    }

    method GetDebuggerName {} {
        return ${debuggerName}
    }

    method SetShortName {name} {
        set shortName ${name}
    }

    method GetShortName {} {
        return ${shortName}
    }

    method GetLongName {} {
	return $fulldescription
    }

    method SetLongName {name} {
	set fulldescription $name
    }
}

##############################################
# CreateFlagIDTag
#
# Since you can't have spaces in an array, we create and IDTag
# for the string which can be later reterived when it is needed.
#

proc CreateFlagIDTag {id_tag textstr} {
    global FlagTags
    set FlagTags(${id_tag}) ${textstr}
}

#Create Toolchain flags and strings.

CreateFlagIDTag DBG_DEFAULT "Compiler Default"
CreateFlagIDTag DBG_NONE "Disabled"
CreateFlagIDTag DBG_STD "Standard Debug"
CreateFlagIDTag DBG_OLD "Old Style Debug"

#OPTIMIZATION FLAGS
CreateFlagIDTag OPT_DEFAULT "Compiler Default"
CreateFlagIDTag OPT_NONE "Disabled"
CreateFlagIDTag OPT_L1 "Level 1"
CreateFlagIDTag OPT_L2 "Level 2"
CreateFlagIDTag OPT_L3 "Level 3 (high)"
CreateFlagIDTag OPT_SPEED "for maximum speed"
CreateFlagIDTag OPT_SIZE "for minimum size only"
CreateFlagIDTag OPT_BOTH "best speed and size"

#CODE GENERATION FLAGS
CreateFlagIDTag CG_DEFAULT "Compiler Default"
CreateFlagIDTag CG_BLEND "Bended Model"
CreateFlagIDTag CG_386 "Optimize for 80386"
CreateFlagIDTag CG_486 "Optimize for 80486"
CreateFlagIDTag CG_PENT "Optimize for Pentium"
CreateFlagIDTag CG_PPRO "Optimize for Pentium Pro"
CreateFlagIDTag CG_WINAPP "Optimize for Win32 App"
CreateFlagIDTag CG_DLL "Optimize for Win32 DLL"
CreateFlagIDTag CG_SHORTENUMS "Use short enums"
CreateFlagIDTag CG_SHAREDATA "Use Shared Data"
CreateFlagIDTag CG_PIC "Generate position-independent code"
CreateFlagIDTag CG_NONULL "Assume refs not null"
CreateFlagIDTag CG_INLINE "Inline functions where possible"
CreateFlagIDTag CG_NOBOUND "No array bound checking"

#WARNING FLAGS
CreateFlagIDTag WN_DEFAULT "Compiler Default"
CreateFlagIDTag WN_L0 "Level 0"
CreateFlagIDTag WN_L1 "Level 1"
CreateFlagIDTag WN_L2 "Level 2"
CreateFlagIDTag WN_L3 "Level 3"
CreateFlagIDTag WN_L4 "Level 4"
CreateFlagIDTag WN_ERR "Warnings as Errors"
CreateFlagIDTag WN_ANSI "Strict ANSI Warnings"
CreateFlagIDTag WN_STD "Standard Warnings"
CreateFlagIDTag WN_NONE "No Warnings"
CreateFlagIDTag WN_ALL "Report All Warnings"
CreateFlagIDTag WN_STRICT_JAVA "Strict Java Warnings"

proc InitializeToolChainInfo {} {

    global sn_path
    # TODO: We need to add support to check
    #       a user toolchain directory so that
    #       users don't have to install toolchain
    #       spex in .../share/etc/sn_toolchains

    # Check .../share/etc/sn_toolchains for toolchain spex.
    
    set toolchain_dir [file join [pwd] $sn_path(toolchaindir)]

    set toolchain_files [sn_glob $toolchain_dir]

    foreach file $toolchain_files {
        if {![file isdirectory $file]} {
            source $file
	}
    }
}

proc GetActiveToolChains {} {
    
    set installed ""

    foreach toolchain [itcl::find objects -class snToolChain] {
	# TODO: Test toolchain is installed
        lappend installed $toolchain
    }
    return $installed
}

proc GetToolChainObject {tool_chain_name} {

    set toolchains [GetActiveToolChains]

    foreach toolchain $toolchains {
	if {[$toolchain GetLongName] == $tool_chain_name} {
	    return $toolchain
	}
    }

    # FIXME: To avoid issues just return GNUPro and pray...
    return GNUPro
}

proc DummyProcessArgs {{args ""}} {
    return ""
}

proc sn_add_simple_ide_rule {ident args} {

    set suffixList ""
    set basicAction ""
    set tool ""
    set description ""
    set fileType ${ident}

    # For the time being we are only supporting one
    # toolchain at a time.  So we will just default
    # to GNUPro 
    #set toolchain GNUPro

    for {set i 0} {${i} < [llength ${args}]} {incr i} {
        set arg [lindex ${args} ${i}]
        incr i
        set val [lindex ${args} ${i}]

        switch -- ${arg} {
            "-suffix-list" {
                    set suffixList ${val}
                }
            "-action" {
                    set basicAction ${val}
                }
            "-tool" {
                    set tool ${val}
                }
            "-description" {
                    set description ${val}
                }
            "-file-type" {
                    set fileType ${val}
                }
        }
    }

    if {${suffixList}!=""} {
        set firstSuffix [lindex ${suffixList} 0]
    } else {
        #TODO: error!!!
    }

    #TODO: check other flags to make sure we have enough to
    # create the toolchain properly

    set toolchains [itcl_info objects -class snToolChain]

    foreach toolchain ${toolchains} {
        #Create tool spec.
        ${toolchain} AddRule ${ident} ${firstSuffix} ${tool} ${basicAction}
        ${toolchain} AddRuleInfo ${ident} ${fileType} ${description}

        # Add any remaining suffixes
        foreach suffix ${suffixList} {
            if {${suffix}!=${firstSuffix}} {
                ${toolchain} AddRuleSuffix ${ident} ${suffix}
            }
        }

        # Set other dummy stuff that we won't handle.
        ${toolchain} SetIncludeSpecProc ${ident} DummyProcessArgs
        ${toolchain} SetDefinesSpecProc ${ident} DummyProcessArgs

        # Set default settings for Optimize, Debug, Warnings and
        # Code Generation.  This make the configure tool dialog
        # work without having to change it.    
        ${toolchain} AddRuleFlagSet ${ident} Optimize "" OPT_DEFAULT
        ${toolchain} AddRuleFlagSet ${ident} Debug "" DBG_DEFAULT
        ${toolchain} AddRuleFlagSet ${ident} Warning "" WN_DEFAULT
        ${toolchain} AddRuleFlagSet ${ident} CodeGen "" CG_DEFAULT
    }
}

