# -- Machine type PW
# mark_description "Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z";
# mark_description "-long_double -S -xW";
	.ident "Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z"
	.ident "-long_double -S -xW"
	.file "copy_test2.c"
	.text
	.data
	.align 4
	.bss
	.align 4
	.ident "-?comment:Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z  : copy_test2.c : -long_double -S -xW"
	.data
	.text
# -- Begin  get_rt
# mark_begin;
       .align    4,0x90
	.globl   get_rt
get_rt:
.B1.1:                          # Preds .B1.0
        rdtsc 
	                                                        #7.0
        ret                                                     #11.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	get_rt,@function
	.size	get_rt,.-get_rt
	.data
# -- End  get_rt
	.data
	.text
# -- Begin  copy_
# mark_begin;
       .align    4,0x90
# parameter 1: 16 + %esp
# parameter 2: 20 + %esp
# parameter 3: 24 + %esp
	.globl   copy_
copy_:
.B2.1:                          # Preds .B2.0
        pushl     %edi                                          #20.20
        pushl     %esi                                          #20.20
        pushl     %ebp                                          #20.20
        movl      16(%esp), %ebp                                #18.6
        movl      20(%esp), %ecx                                #18.6
        movl      24(%esp), %edx                                #18.6
        movl      (%ebp), %esi                                  #23.17
        xorl      %eax, %eax                                    #23.7
        lea       -16(%esi), %edi                               #23.17
        testl     %edi, %edi                                    #23.3
        jl        .B2.5         # Prob 2%                       #23.3
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp esi
.B2.3:                          # Preds .B2.1 .B2.3
		#prefetchnta 384(%ecx,%eax,8)
        movapd     (%ecx,%eax,8), %xmm0                          #23.9
        movapd     16(%ecx,%eax,8), %xmm1                        #25.11
        movapd     32(%ecx,%eax,8), %xmm2                        #27.11
        movapd     48(%ecx,%eax,8), %xmm3                        #29.11
        movapd     64(%ecx,%eax,8), %xmm4                        #31.11
        movapd     80(%ecx,%eax,8), %xmm5                        #33.12
        movapd     96(%ecx,%eax,8), %xmm6                        #35.12
        movapd     112(%ecx,%eax,8), %xmm7                       #37.12
        movntpd     %xmm0, (%edx,%eax,8)                          #23.2
        movntpd     %xmm1, 16(%edx,%eax,8)                        #25.2
        movntpd     %xmm2, 32(%edx,%eax,8)                        #27.2
        movntpd     %xmm3, 48(%edx,%eax,8)                        #29.2
        movntpd     %xmm4, 64(%edx,%eax,8)                        #31.2
        movntpd     %xmm5, 80(%edx,%eax,8)                        #33.2
        movntpd     %xmm6, 96(%edx,%eax,8)                        #35.2
        movntpd     %xmm7, 112(%edx,%eax,8)                       #37.2
        addl      $16, %eax                                     #23.30
        movl      (%ebp), %esi                                  #23.17
        lea       -16(%esi), %edi                               #23.17
        cmpl      %edi, %eax                                    #23.3
        jle       .B2.3         # Prob 97%                      #23.3
                                # LOE eax edx ecx ebx ebp esi
.B2.5:                          # Preds .B2.3 .B2.1
        cmpl      %esi, %eax                                    #42.3
        jge       .B2.9         # Prob 2%                       #42.3
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp
.B2.7:                          # Preds .B2.5 .B2.7
        movsd     (%ecx,%eax,8), %xmm0                          #43.9
        movsd     %xmm0, (%edx,%eax,8)                          #43.2
        addl      $1, %eax                                      #42.19
        cmpl      (%ebp), %eax                                  #42.3
        jl        .B2.7         # Prob 97%                      #42.3
                                # LOE eax edx ecx ebx ebp
.B2.9:                          # Preds .B2.7 .B2.5
        popl      %ebp                                          #46.1
        popl      %esi                                          #46.1
        popl      %edi                                          #46.1
        ret                                                     #46.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	copy_,@function
	.size	copy_,.-copy_
	.data
# -- End  copy_
	.data
_2il0floatpacket.7:
	.long 0x9999999a,0x3fb99999,0x9999999a,0x3fb99999	# md128
	.type	_2il0floatpacket.7,@object
	.size	_2il0floatpacket.7,16
_2il0floatpacket.3:
	.long 0x00000000,0x40000000,0x00000000,0x40000000	# md128
	.type	_2il0floatpacket.3,@object
	.size	_2il0floatpacket.3,16
_2il0floatpacket.11:
	.long 0x00000000,0x409f4000	# xf64
	.type	_2il0floatpacket.11,@object
	.size	_2il0floatpacket.11,8
_2il0floatpacket.9:
	.long 0x9999999a,0x3fb99999	# xf64
	.type	_2il0floatpacket.9,@object
	.size	_2il0floatpacket.9,8
_2il0floatpacket.5:
	.long 0x00000000,0x3ff00000	# xf64
	.type	_2il0floatpacket.5,@object
	.size	_2il0floatpacket.5,8
_2il0floatpacket.1:
	.long 0x00000000,0x00000000	# xf64
	.type	_2il0floatpacket.1,@object
	.size	_2il0floatpacket.1,8
__STRING.2:
	.byte 37	# s8
	.byte 105	# s8
	.byte 44	# s8
	.byte 32	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.2,@object
	.size	__STRING.2,8
__STRING.1:
	.byte 115	# s8
	.byte 114	# s8
	.byte 99	# s8
	.byte 91	# s8
	.byte 37	# s8
	.byte 105	# s8
	.byte 93	# s8
	.byte 61	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 32	# s8
	.byte 33	# s8
	.byte 61	# s8
	.byte 32	# s8
	.byte 100	# s8
	.byte 115	# s8
	.byte 116	# s8
	.byte 91	# s8
	.byte 37	# s8
	.byte 105	# s8
	.byte 93	# s8
	.byte 61	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.1,@object
	.size	__STRING.1,26
	.space 2	# pad
__STRING.0:
	.byte 98	# s8
	.byte 108	# s8
	.byte 111	# s8
	.byte 99	# s8
	.byte 107	# s8
	.byte 32	# s8
	.byte 115	# s8
	.byte 105	# s8
	.byte 122	# s8
	.byte 101	# s8
	.byte 44	# s8
	.byte 32	# s8
	.byte 77	# s8
	.byte 66	# s8
	.byte 121	# s8
	.byte 116	# s8
	.byte 101	# s8
	.byte 47	# s8
	.byte 115	# s8
	.byte 101	# s8
	.byte 99	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.0,@object
	.size	__STRING.0,23
	.data
	.text
# -- Begin  main
# mark_begin;
       .align    4,0x90
	.globl   main
main:
.B3.1:                          # Preds .B3.0
        pushl     %ebx                                          #50.13
        movl      %esp, %ebx                                    #50.13
        andl      $-16, %esp                                    #50.13
        pushl     %esi                                          #50.13
        pushl     %ebp                                          #50.13
        subl      $72, %esp                                     #50.13
        pushl     %eax                                          #50.13
        pushl     %eax                                          #50.13
        stmxcsr   (%esp)                                        #50.13
        popl      %eax                                          #50.13
        orl       $32768, %eax                                  #50.13
        pushl     %eax                                          #50.13
        ldmxcsr   (%esp)                                        #50.13
        popl      %eax                                          #50.13
        popl      %eax                                          #50.13
        push      $536870920                                    #56.8
        call      malloc                                        #56.8
                                # LOE eax edi
.B3.34:                         # Preds .B3.1
        movl      %eax, %esi                                    #56.8
                                # LOE esi edi
.B3.2:                          # Preds .B3.34
        push      $536870920                                    #57.8
        call      malloc                                        #57.8
                                # LOE eax esi edi
.B3.35:                         # Preds .B3.2
        movl      %eax, %ebp                                    #57.8
        addl      $8, %esp                                      #57.8
                                # LOE ebp esi edi
.B3.3:                          # Preds .B3.35
        lea       8(%esi), %edx                                 #62.2
        movl      %esi, %ecx                                    #60.13
        andl      $15, %ecx                                     #60.13
        cmovne    %edx, %esi                                    #62.2
        lea       8(%ebp), %edx                                 #66.2
        movl      %esi, %eax                                    #69.3
        movl      %ebp, %ecx                                    #64.13
        andl      $15, %ecx                                     #64.13
        cmovne    %edx, %ebp                                    #66.2
        movl      $67108864, %edx                               #69.3
        andl      $15, %eax                                     #69.3
        movl      $0, %ecx                                      #69.3
        je        .B3.6         # Prob 50%                      #69.3
                                # LOE eax edx ecx ebp esi edi
.B3.4:                          # Preds .B3.3
        testb     $7, %al                                       #69.3
        jne       .B3.12        # Prob 0%                       #69.3
                                # LOE ecx ebp esi edi
.B3.5:                          # Preds .B3.4
        movl      $67108863, %edx                               #69.3
        movl      $1, %eax                                      #69.3
                                # LOE eax edx ecx ebp esi edi
.B3.6:                          # Preds .B3.3 .B3.5
        andl      $7, %edx                                      #69.3
        negl      %edx                                          #69.3
        addl      $67108864, %edx                               #69.3
        testl     %eax, %eax                                    #69.3
        je        .B3.8         # Prob 50%                      #69.3
                                # LOE edx ecx ebp esi edi
.B3.7:                          # Preds .B3.6
        movsd     _2il0floatpacket.1, %xmm0                     #70.2
        movsd     %xmm0, (%esi)                                 #70.2
        movl      $1, %ecx                                      #69.3
                                # LOE edx ecx ebp esi edi
.B3.8:                          # Preds .B3.6 .B3.7
        cvtsi2sd  %ecx, %xmm1                                   #69.22
        movsd     _2il0floatpacket.5, %xmm2                     #70.20
        movapd    _2il0floatpacket.3, %xmm0                     #70.20
        addsd     %xmm1, %xmm2                                  #70.20
        movsd     %xmm2, 40(%esp)                               #70.20
        xorpd     %xmm2, %xmm2                                  #70.20
        movsd     %xmm1, %xmm2                                  #70.20
        movhpd    40(%esp), %xmm2                               #70.20
        movapd    _2il0floatpacket.7, %xmm1                     #70.20
        .align    4,0x90
                                # LOE edx ecx ebp esi edi xmm0 xmm1 xmm2
.B3.9:                          # Preds .B3.9 .B3.8
        movapd    %xmm2, %xmm3                                  #70.20
        mulpd     %xmm1, %xmm3                                  #70.20
        movapd    %xmm3, (%esi,%ecx,8)                          #70.2
        addpd     %xmm0, %xmm2                                  #70.20
        movapd    %xmm2, %xmm4                                  #70.20
        mulpd     %xmm1, %xmm4                                  #70.20
        movapd    %xmm4, 16(%esi,%ecx,8)                        #70.2
        addpd     %xmm0, %xmm2                                  #70.20
        movapd    %xmm2, %xmm5                                  #70.20
        mulpd     %xmm1, %xmm5                                  #70.20
        movapd    %xmm5, 32(%esi,%ecx,8)                        #70.2
        addpd     %xmm0, %xmm2                                  #70.20
        movapd    %xmm2, %xmm6                                  #70.20
        mulpd     %xmm1, %xmm6                                  #70.20
        movapd    %xmm6, 48(%esi,%ecx,8)                        #70.2
        addpd     %xmm0, %xmm2                                  #70.20
        addl      $8, %ecx                                      #69.3
        cmpl      %edx, %ecx                                    #69.3
        jl        .B3.9         # Prob 100%                     #69.3
                                # LOE edx ecx ebp esi edi xmm0 xmm1 xmm2
.B3.10:                         # Preds .B3.9
        cmpl      $67108864, %ecx                               #69.3
        jge       .B3.14        # Prob 50%                      #69.3
        .align    4,0x90
                                # LOE ecx ebp esi edi
.B3.12:                         # Preds .B3.4 .B3.10 .B3.12
        cvtsi2sd  %ecx, %xmm0                                   #70.20
        mulsd     _2il0floatpacket.9, %xmm0                     #70.20
        movsd     %xmm0, (%esi,%ecx,8)                          #70.2
        addl      $1, %ecx                                      #69.3
        cmpl      $67108864, %ecx                               #69.3
        jl        .B3.12        # Prob 50%                      #69.3
                                # LOE ecx ebp esi edi
.B3.14:                         # Preds .B3.12 .B3.10
        push      $__STRING.0                                   #73.3
        call      printf                                        #73.3
                                # LOE ebp esi edi
.B3.36:                         # Preds .B3.14
        popl      %ecx                                          #73.3
                                # LOE ebp esi edi
.B3.15:                         # Preds .B3.36
        movl      $1, 32(%esp)                                  #74.8
        movl      %ebp, 52(%esp)                                #74.8
        movl      %esi, 48(%esp)                                #74.8
        movl      %edi, 68(%esp)                                #74.8
        jmp       .B3.16        # Prob 100%                     #74.8
                                # LOE
.B3.28:                         # Preds .B3.27
        movl      %eax, 32(%esp)                                #74.8
                                # LOE
.B3.16:                         # Preds .B3.15 .B3.28
        call      get_rt                                        #78.7
                                # LOE eax
.B3.38:                         # Preds .B3.16
        movl      %edx, %ebp                                    #78.7
        movl      %eax, %esi                                    #78.7
                                # LOE ebp esi
.B3.17:                         # Preds .B3.38
        movl      32(%esp), %edx                                #79.20
        xorl      %edi, %edi                                    #79.6
        negl      %edx                                          #79.11
        addl      $67108864, %edx                               #79.11
        testl     %edx, %edx                                    #79.2
        jl        .B3.22        # Prob 2%                       #79.2
                                # LOE ebp esi edi
.B3.18:                         # Preds .B3.17
        movl      %ebp, 64(%esp)                                #
        movl      52(%esp), %ebp                                #
        movl      %esi, 60(%esp)                                #
        movl      48(%esp), %esi                                #
        .align    4,0x90
                                # LOE ebp esi edi
.B3.19:                         # Preds .B3.18 .B3.20
        lea       32(%esp), %eax                                #81.10
        lea       (%edi,%edi), %edx                             #81.16
        addl      %edx, %edx                                    #81.16
        addl      %edx, %edx                                    #81.16
        lea       (%esi,%edx), %ecx                             #81.16
        addl      %ebp, %edx                                    #81.23
        pushl     %edx                                          #81.23
        pushl     %ecx                                          #81.23
        pushl     %eax                                          #81.23
        call      copy_                                         #81.23
                                # LOE ebp esi edi
.B3.39:                         # Preds .B3.19
        addl      $12, %esp                                     #81.23
                                # LOE ebp esi edi
.B3.20:                         # Preds .B3.39
        addl      32(%esp), %edi                                #79.25
        movl      32(%esp), %edx                                #79.28
        negl      %edx                                          #79.20
        addl      $67108864, %edx                               #79.20
        cmpl      %edx, %edi                                    #79.2
        jle       .B3.19        # Prob 90%                      #79.2
                                # LOE ebp esi edi
.B3.21:                         # Preds .B3.20
        movl      64(%esp), %ebp                                #
        movl      60(%esp), %esi                                #
                                # LOE ebp esi edi
.B3.22:                         # Preds .B3.21 .B3.17
        call      get_rt                                        #83.7
                                # LOE eax ebp esi edi
.B3.41:                         # Preds .B3.22
        movl      %edx, 56(%esp)                                #83.7
        movl      %eax, %ecx                                    #83.7
                                # LOE ecx ebp esi edi
.B3.23:                         # Preds .B3.41
        addl      %edi, %edi                                    #85.7
        addl      %edi, %edi                                    #85.7
        addl      %edi, %edi                                    #85.7
        addl      %edi, %edi                                    #85.7
        movl      %edi, %eax                                    #85.7
        cltd                                                    #85.7
        xorl      %eax, %eax                                    #87.6
        subl      %esi, %ecx                                    #84.11
        movl      56(%esp), %esi                                #84.11
        sbbl      %ebp, %esi                                    #84.11
        movl      48(%esp), %ebp                                #87.6
        movl      %esi, 56(%esp)                                #84.11
        movl      52(%esp), %esi                                #87.6
        .align    4,0x90
                                # LOE eax edx ecx ebp esi edi
.B3.24:                         # Preds .B3.25 .B3.23
        movsd     (%ebp,%eax,8), %xmm0                          #88.7
        movsd     (%esi,%eax,8), %xmm1                          #88.17
        comisd    %xmm1, %xmm0                                  #88.4
        jne       .B3.29        # Prob 3%                       #88.4
                                # LOE eax edx ecx ebp esi edi xmm0 xmm1
.B3.25:                         # Preds .B3.24
        addl      $1, %eax                                      #87.20
        cmpl      $67108864, %eax                               #87.2
        jl        .B3.24        # Prob 100%                     #87.2
                                # LOE eax edx ecx ebp esi edi
.B3.26:                         # Preds .B3.25
        movl      56(%esp), %eax                                #95.24
        movl      $__STRING.2, (%esp)                           #96.40
        movl      %edi, 40(%esp)                                #95.11
        movl      %edx, 44(%esp)                                #95.11
        fildll    40(%esp)                                      #95.11
        fmull     _2il0floatpacket.11                           #95.11
        movl      32(%esp), %edx                                #96.21
        movl      %ecx, 40(%esp)                                #95.24
        addl      %edx, %edx                                    #96.21
        addl      %edx, %edx                                    #96.21
        movl      %eax, 44(%esp)                                #95.24
        fildll    40(%esp)                                      #95.24
        fdivrp    %st, %st(1)                                   #95.24
        fstpl     8(%esp)                                       #96.40
        addl      %edx, %edx                                    #96.21
        movl      %edx, 4(%esp)                                 #96.40
        call      printf                                        #96.40
                                # LOE
.B3.27:                         # Preds .B3.26
        movl      32(%esp), %eax                                #74.27
        addl      %eax, %eax                                    #74.27
        cmpl      $67108864, %eax                               #74.3
        jl        .B3.28        # Prob 90%                      #74.3
        jmp       .B3.31        # Prob 100%                     #74.3
        .align    4,0x90
                                # LOE eax
.B3.29:                         # Preds .B3.24                  # Infreq
        movsd     %xmm0, 8(%esp)                                #90.54
        movsd     %xmm1, 20(%esp)                               #90.54
        movl      %eax, 4(%esp)                                 #90.54
        movl      $__STRING.1, (%esp)                           #90.54
        movl      %eax, 16(%esp)                                #90.54
        call      printf                                        #90.54
                                # LOE
.B3.30:                         # Preds .B3.29                  # Infreq
        push      $-1                                           #91.5
        call      exit                                          #91.5
                                # LOE
.B3.42:                         # Preds .B3.30                  # Infreq
        popl      %ecx                                          #91.5
                                # LOE
.B3.31:                         # Preds .B3.27                  # Infreq
        movl      68(%esp), %edi                                #
        xorl      %eax, %eax                                    #98.1
        addl      $72, %esp                                     #98.1
        popl      %ebp                                          #98.1
        popl      %esi                                          #98.1
        movl      %ebx, %esp                                    #98.1
        popl      %ebx                                          #98.1
        ret                                                     #98.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	main,@function
	.size	main,.-main
	.data
# -- End  main
	.data
# End
