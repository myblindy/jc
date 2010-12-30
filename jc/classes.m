stack res 5000
align
fn__C_f_ nop
addi r1,r0,5
addi r2,r14,68
sw stack(r2),r1
addi r1,r14,68
lw r2,stack(r1)
addi r1,r14,0
sw stack(r1), r2
addi r1,r0,2
addi r2,r14,72
sw stack(r2),r1
addi r1,r14,72
lw r2,stack(r1)
addi r1,r14,8
sw stack(r1), r2
addi r1,r0,10
addi r2,r14,76
sw stack(r2),r1
addi r3,r14,8
lw r1,stack(r3)
addi r3,r14,76
lw r2,stack(r3)
mul r1,r1,r2
addi r2,r14,76
sw stack(r2),r1
addi r2,r12,4
lw r1,stack(r2)
addi r2,r14,68
sw stack(r2),r1
addi r3,r14,76
lw r1,stack(r3)
addi r3,r14,68
lw r2,stack(r3)
add r1,r1,r2
addi r2,r14,68
sw stack(r2),r1
addi r3,r14,68
lw r1,stack(r3)
addi r3,r14,0
lw r2,stack(r3)
mul r1,r1,r2
addi r2,r14,68
sw stack(r2),r1
addi r3,r14,68
lw r1,stack(r3)
addi r3,r14,4
lw r2,stack(r3)
mul r1,r1,r2
addi r2,r14,68
sw stack(r2),r1
addi r2,r14,68
lw r1,stack(r2)
jr r15
% append default return
addi r1,r0,0
jr r15
entry

align
addi r14,r0,0
addi r1,r0,3
addi r2,r14,16
sw stack(r2),r1
addi r1,r14,16
lw r2,stack(r1)
addi r1,r14,8
sw stack(r1), r2
addi r1,r0,5
addi r2,r14,20
sw stack(r2),r1
addi r1,r14,20
lw r2,stack(r1)
addi r1,r14,12
sw stack(r1), r2
addi r1,r0,1
addi r2,r14,24
sw stack(r2),r1
addi r1,r14,24
lw r2,stack(r1)
addi r1,r14,4
sw stack(r1), r2
add r13,r0,r14
addi r14,r14,32
%pusha
addi r1,r14,12
sw stack(r1),r2 
addi r1,r14,16
sw stack(r1),r3 
addi r1,r14,20
sw stack(r1),r4 
addi r1,r14,24
sw stack(r1),r5 
addi r1,r14,28
sw stack(r1),r6 
addi r1,r14,32
sw stack(r1),r7 
addi r1,r14,36
sw stack(r1),r8 
addi r1,r14,40
sw stack(r1),r9 
addi r1,r14,44
sw stack(r1),r10 
addi r1,r14,48
sw stack(r1),r11 
addi r1,r14,52
sw stack(r1),r12 
addi r1,r14,56
sw stack(r1),r13 
addi r1,r14,60
sw stack(r1),r14 
addi r1,r14,64
sw stack(r1),r15 
addi r2,r13,4
lw r1,stack(r2)
addi r2,r14,4
sw stack(r2),r1
addi r12,r13,8
jl r15,fn__C_f_
sw stack(r14),r1
%popa
addi r1,r14,12
lw r2,stack(r1)
addi r1,r14,16
lw r3,stack(r1)
addi r1,r14,20
lw r4,stack(r1)
addi r1,r14,24
lw r5,stack(r1)
addi r1,r14,28
lw r6,stack(r1)
addi r1,r14,32
lw r7,stack(r1)
addi r1,r14,36
lw r8,stack(r1)
addi r1,r14,40
lw r9,stack(r1)
addi r1,r14,44
lw r10,stack(r1)
addi r1,r14,48
lw r11,stack(r1)
addi r1,r14,52
lw r12,stack(r1)
addi r1,r14,56
lw r13,stack(r1)
addi r1,r14,60
lw r14,stack(r1)
addi r1,r14,64
lw r15,stack(r1)
subi r14,r14,32
addi r2,r14,32
lw r1,stack(r2)
addi r2,r14,28
sw stack(r2),r1
addi r1,r14,28
lw r2,stack(r1)
addi r1,r14,0
sw stack(r1), r2
%pusha
addi r1,r14,32
sw stack(r1),r2 
addi r1,r14,36
sw stack(r1),r3 
addi r1,r14,40
sw stack(r1),r4 
addi r1,r14,44
sw stack(r1),r5 
addi r1,r14,48
sw stack(r1),r6 
addi r1,r14,52
sw stack(r1),r7 
addi r1,r14,56
sw stack(r1),r8 
addi r1,r14,60
sw stack(r1),r9 
addi r1,r14,64
sw stack(r1),r10 
addi r1,r14,68
sw stack(r1),r11 
addi r1,r14,72
sw stack(r1),r12 
addi r1,r14,76
sw stack(r1),r13 
addi r1,r14,80
sw stack(r1),r14 
addi r1,r14,84
sw stack(r1),r15 
addi r2,r14,0
lw r1,stack(r2)
jl r15,putint
%popa
addi r1,r14,32
lw r2,stack(r1)
addi r1,r14,36
lw r3,stack(r1)
addi r1,r14,40
lw r4,stack(r1)
addi r1,r14,44
lw r5,stack(r1)
addi r1,r14,48
lw r6,stack(r1)
addi r1,r14,52
lw r7,stack(r1)
addi r1,r14,56
lw r8,stack(r1)
addi r1,r14,60
lw r9,stack(r1)
addi r1,r14,64
lw r10,stack(r1)
addi r1,r14,68
lw r11,stack(r1)
addi r1,r14,72
lw r12,stack(r1)
addi r1,r14,76
lw r13,stack(r1)
addi r1,r14,80
lw r14,stack(r1)
addi r1,r14,84
lw r15,stack(r1)
%pusha
addi r1,r14,32
sw stack(r1),r2 
addi r1,r14,36
sw stack(r1),r3 
addi r1,r14,40
sw stack(r1),r4 
addi r1,r14,44
sw stack(r1),r5 
addi r1,r14,48
sw stack(r1),r6 
addi r1,r14,52
sw stack(r1),r7 
addi r1,r14,56
sw stack(r1),r8 
addi r1,r14,60
sw stack(r1),r9 
addi r1,r14,64
sw stack(r1),r10 
addi r1,r14,68
sw stack(r1),r11 
addi r1,r14,72
sw stack(r1),r12 
addi r1,r14,76
sw stack(r1),r13 
addi r1,r14,80
sw stack(r1),r14 
addi r1,r14,84
sw stack(r1),r15 
addi r2,r14,8
lw r1,stack(r2)
jl r15,putint
%popa
addi r1,r14,32
lw r2,stack(r1)
addi r1,r14,36
lw r3,stack(r1)
addi r1,r14,40
lw r4,stack(r1)
addi r1,r14,44
lw r5,stack(r1)
addi r1,r14,48
lw r6,stack(r1)
addi r1,r14,52
lw r7,stack(r1)
addi r1,r14,56
lw r8,stack(r1)
addi r1,r14,60
lw r9,stack(r1)
addi r1,r14,64
lw r10,stack(r1)
addi r1,r14,68
lw r11,stack(r1)
addi r1,r14,72
lw r12,stack(r1)
addi r1,r14,76
lw r13,stack(r1)
addi r1,r14,80
lw r14,stack(r1)
addi r1,r14,84
lw r15,stack(r1)
hlt

putint	align
	add	r2,r0,r0		% Initialize buffer's index i
	cge	r3,r1,r0		% True if N >= 0
	bnz	r3,putint1		% Branch if True (N >= 0)
	sub	r1,r0,r1		% N = -N
putint1	modi	r4,r1,10		% Rightmost digit
	addi	r4,r4,48		% Convert to ch
	divi	r1,r1,10		% Truncate rightmost digit
	sb	putint9(r2),r4		% Store ch in buffer
	addi	r2,r2,1			% i++
	bnz	r1,putint1		% Loop if not finished
	bnz	r3,putint2		% Branch if True (N >= 0)
	addi	r3,r0,45
	sb	putint9(r2),r3		% Store '-' in buffer
	addi	r2,r2,1			% i++
	add	r1,r0,r0		% Initialize output register (r1 = 0)
putint2	subi	r2,r2,1			% i--
	lb	r1,putint9(r2)		% Load ch from buffer
	putc	r1			% Output ch
	bnz	r2,putint2		% Loop if not finished
    addi r1,r0,13       % load new line
    putc r1             % print it
    addi r1,r0,10       % new feed thingie
    putc r1
	jr	r15			% return to the caller
putint9	res	12			% loacl buffer (12 bytes)
	align

getint   add    r1,r0,r0         % n := 0 (result)
         add    r2,r0,r0         % c := 0 (character)
         add    r3,r0,r0         % s := 0 (sign)
getint1  getc   r2               % read c
         ceqi   r4,r2,32
         bnz    r4,getint1       % skip blanks
         ceqi   r4,r2,43
         bnz    r4,getint2       % branch if c is '+'
         ceqi   r4,r2,45
         bz     r4,getint3       % branch if c is not '-'
         addi   r3,r0,1          % s := 1 (number is negative)
getint2  getc   r2               % read c
getint3  ceqi   r4,r2,10
         bnz    r4,getint5       % branch if c is \n
         cgei   r4,r2,48
         bz     r4,getint4       % c < 0
         clei   r4,r2,57
         bz     r4,getint4       % c > 9
         muli   r1,r1,10         % n := 10 * n
         add    r1,r1,r2         % n := n + c
         subi   r1,r1,48         % n := n - '0'
         j      getint2
getint4  addi   r2,r0,63         % c := '?'
         putc   r2               % write c
         j      getint           % Try again
getint5  bz     r3,getint6       % branch if s = 0 (number is positive)
         sub    r1,r0,r1         % n := -n
getint6  jr     r15              % return

