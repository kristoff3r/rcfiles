set disassembly-flavor intel
set disable-randomization off
set pagination off
set history filename ~/.gdbhistory
set history save
set history expansion
start
display/30i $eip
