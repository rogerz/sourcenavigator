# Tcl package index file, version 1.0

package ifneeded Tix 4.1.7.5 \
    [list load "[file join [file dirname $dir] tix4175.dll]" Tix]

package ifneeded Tix 4.1.7.6 \
    [list load "[file join [file dirname $dir] tix4176.dll]" Tix]

# Itcl 2.2
package ifneeded Tix 4.1.7.6.1 \
    [list load "[file join [file dirname $dir] tix41761.dll]" Tix]

package ifneeded Tix 4.1.8.0 \
    [list load "[file join [file dirname $dir] tix4180.dll]" Tix]

