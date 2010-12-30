stack res 5000
entry

sub r5,r5,r5
addi r5,r5,0
lw r1,stack(r5)
sub r2,r2,r2
add r2,r2,r1
sub r5,r5,r5
addi r5,r5,0
lw r1,stack(r5)
add r1,r2,r1
sub r5,r5,r5
addi r5,r5,4
sw stack(r5), r1
hlt