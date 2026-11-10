# -- Machine type PW
# mark_description "Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z";
# mark_description "-long_double -S -xW";
	.ident "Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z"
	.ident "-long_double -S -xW"
	.file "trans_test.c"
	.text
	.data
	.align 4
	.bss
	.align 4
	.ident "-?comment:Intel(R) C++ Compiler for 32-bit applications, Version 6.0   Build 020722Z  : trans_test.c : -long_double -S -xW"
	.data
	.text
# -- Begin  dummy
# mark_begin;
       .align    4,0x90
	.globl   dummy
dummy:
.B1.1:                          # Preds .B1.0
        movl      $1, %eax                                      #5.3
        xorl      %edx, %edx                                    #5.3
        ret                                                     #5.3
        .align    4,0x90
                                # LOE
# mark_end;
	.type	dummy,@function
	.size	dummy,.-dummy
	.data
# -- End  dummy
	.data
	.text
# -- Begin  get_rt
# mark_begin;
       .align    4,0x90
	.globl   get_rt
get_rt:
.B2.1:                          # Preds .B2.0
        rdtsc 
	                                                        #9.0
        ret                                                     #13.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	get_rt,@function
	.size	get_rt,.-get_rt
	.data
# -- End  get_rt
	.data
	.text
# -- Begin  transp_
# mark_begin;
       .align    4,0x90
# parameter 1: 16936 + %esp
# parameter 2: 16940 + %esp
# parameter 3: 16944 + %esp
# parameter 4: 16948 + %esp
	.globl   transp_
transp_:
.B3.1:                          # Preds .B3.0
        pushl     %edi                                          #133.58
        pushl     %esi                                          #133.58
        pushl     %ebp                                          #133.58
        pushl     %ebx                                          #133.58
        subl      $16916, %esp                                  #133.58
        movl      16936(%esp), %ebp                             #133.6
        xorl      %edx, %edx                                    #136.3
        xorl      %eax, %eax                                    #137.8
        movl      16940(%esp), %ecx                             #133.6
        movl      (%ecx), %edi                                  #137.17
        addl      $-32, %edi                                    #137.17
        testl     %edi, %edi                                    #137.3
        jl        .B3.13        # Prob 2%                       #137.3
                                # LOE eax edx ebp edi
.B3.2:                          # Preds .B3.1
        movl      (%ebp), %esi                                  #139.16
        movl      %esi, 16896(%esp)                             #139.16
        lea       -32(%esi), %ecx                               #139.16
                                # LOE eax edx ecx ebx ebp edi
.B3.3:                          # Preds .B3.2 .B3.11
        movl      %eax, %esi                                    #138.2
        xorl      %ebp, %ebp                                    #139.7
        testl     %ecx, %ecx                                    #139.2
        jl        .B3.11        # Prob 2%                       #139.2
                                # LOE eax edx ecx ebx ebp esi edi
.B3.4:                          # Preds .B3.3
        movl      16896(%esp), %edi                             #
        movl      %eax, 16900(%esp)                             #
        movl      16936(%esp), %eax                             #
                                # LOE eax edx ebp esi edi
.B3.5:                          # Preds .B3.4 .B3.9
        movl      %esi, 16904(%esp)                             #141.9
        movl      16944(%esp), %esi                             #141.9
        movl      %edx, %ebx                                    #138.2
        xorl      %ecx, %ecx                                    #141.9
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp esi edi
.B3.6:                          # Preds .B3.6 .B3.5
        lea       (%ebp,%ebx), %eax                             #143.23
        addl      %eax, %eax                                    #143.25
        movsd     (%esi,%eax,8), %xmm1                          #143.17
        movsd     8(%esi,%eax,8), %xmm2                         #143.17
        movsd     16(%esi,%eax,8), %xmm3                        #143.17
        movsd     24(%esi,%eax,8), %xmm4                        #143.17
        movsd     32(%esi,%eax,8), %xmm6                        #143.17
        movsd     40(%esi,%eax,8), %xmm7                        #143.17
        movsd     48(%esi,%eax,8), %xmm0                        #143.17
        movsd     56(%esi,%eax,8), %xmm5                        #143.17
        movsd     %xmm1, (%esp,%ecx)                            #143.5
        movsd     %xmm2, 8(%esp,%ecx)                           #143.5
        movsd     %xmm3, 16(%esp,%ecx)                          #143.5
        movsd     %xmm4, 24(%esp,%ecx)                          #143.5
        movsd     %xmm6, 32(%esp,%ecx)                          #143.5
        movsd     %xmm7, 40(%esp,%ecx)                          #143.5
        movsd     %xmm0, 48(%esp,%ecx)                          #143.5
        movsd     64(%esi,%eax,8), %xmm0                        #143.17
        movsd     %xmm5, 56(%esp,%ecx)                          #143.5
        movsd     %xmm0, 64(%esp,%ecx)                          #143.5
        movsd     72(%esi,%eax,8), %xmm0                        #143.17
        movsd     %xmm0, 72(%esp,%ecx)                          #143.5
        movsd     80(%esi,%eax,8), %xmm0                        #143.17
        movsd     %xmm0, 80(%esp,%ecx)                          #143.5
        movsd     88(%esi,%eax,8), %xmm0                        #143.17
        movsd     %xmm0, 88(%esp,%ecx)                          #143.5
        movsd     96(%esi,%eax,8), %xmm0                        #143.17
        movsd     %xmm0, 96(%esp,%ecx)                          #143.5
        movsd     104(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 104(%esp,%ecx)                         #143.5
        movsd     112(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 112(%esp,%ecx)                         #143.5
        movsd     120(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 120(%esp,%ecx)                         #143.5
        movsd     128(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 128(%esp,%ecx)                         #143.5
        movsd     136(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 136(%esp,%ecx)                         #143.5
        movsd     144(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 144(%esp,%ecx)                         #143.5
        movsd     152(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 152(%esp,%ecx)                         #143.5
        movsd     160(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 160(%esp,%ecx)                         #143.5
        movsd     168(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 168(%esp,%ecx)                         #143.5
        movsd     176(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 176(%esp,%ecx)                         #143.5
        movsd     184(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 184(%esp,%ecx)                         #143.5
        movsd     192(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 192(%esp,%ecx)                         #143.5
        movsd     200(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 200(%esp,%ecx)                         #143.5
        movsd     208(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 208(%esp,%ecx)                         #143.5
        movsd     216(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 216(%esp,%ecx)                         #143.5
        movsd     224(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 224(%esp,%ecx)                         #143.5
        movsd     232(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 232(%esp,%ecx)                         #143.5
        movsd     240(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 240(%esp,%ecx)                         #143.5
        movsd     248(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 248(%esp,%ecx)                         #143.5
        movsd     256(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 256(%esp,%ecx)                         #143.5
        movsd     264(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 264(%esp,%ecx)                         #143.5
        movsd     272(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 272(%esp,%ecx)                         #143.5
        movsd     280(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 280(%esp,%ecx)                         #143.5
        movsd     288(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 288(%esp,%ecx)                         #143.5
        movsd     296(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 296(%esp,%ecx)                         #143.5
        movsd     304(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 304(%esp,%ecx)                         #143.5
        movsd     312(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 312(%esp,%ecx)                         #143.5
        movsd     320(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 320(%esp,%ecx)                         #143.5
        movsd     328(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 328(%esp,%ecx)                         #143.5
        movsd     336(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 336(%esp,%ecx)                         #143.5
        movsd     344(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 344(%esp,%ecx)                         #143.5
        movsd     352(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 352(%esp,%ecx)                         #143.5
        movsd     360(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 360(%esp,%ecx)                         #143.5
        movsd     368(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 368(%esp,%ecx)                         #143.5
        movsd     376(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 376(%esp,%ecx)                         #143.5
        movsd     384(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 384(%esp,%ecx)                         #143.5
        movsd     392(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 392(%esp,%ecx)                         #143.5
        movsd     400(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 400(%esp,%ecx)                         #143.5
        movsd     408(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 408(%esp,%ecx)                         #143.5
        movsd     416(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 416(%esp,%ecx)                         #143.5
        movsd     424(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 424(%esp,%ecx)                         #143.5
        movsd     432(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 432(%esp,%ecx)                         #143.5
        movsd     440(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 440(%esp,%ecx)                         #143.5
        movsd     448(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 448(%esp,%ecx)                         #143.5
        movsd     456(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 456(%esp,%ecx)                         #143.5
        movsd     464(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 464(%esp,%ecx)                         #143.5
        movsd     472(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 472(%esp,%ecx)                         #143.5
        movsd     480(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 480(%esp,%ecx)                         #143.5
        movsd     488(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 488(%esp,%ecx)                         #143.5
        movsd     496(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 496(%esp,%ecx)                         #143.5
        movsd     504(%esi,%eax,8), %xmm0                       #143.17
        movsd     %xmm0, 504(%esp,%ecx)                         #143.5
        addl      %edi, %ebx                                    #145.3
        addl      $512, %ecx                                    #141.23
        cmpl      $16384, %ecx                                  #141.4
        jl        .B3.6         # Prob 96%                      #141.4
                                # LOE edx ecx ebx ebp esi edi
.B3.7:                          # Preds .B3.6
        movl      16904(%esp), %esi                             #
        movl      16948(%esp), %eax                             #147.9
        movl      %ebx, 16908(%esp)                             #147.9
        xorl      %ecx, %ecx                                    #147.9
        movl      16940(%esp), %ebx                             #147.9
        .align    4,0x90
                                # LOE eax edx ecx ebx ebp esi
.B3.8:                          # Preds .B3.8 .B3.7
        movsd     (%esp,%ecx), %xmm0                            #149.16
        movsd     14336(%esp,%ecx), %xmm7                       #149.16
        movsd     14344(%esp,%ecx), %xmm6                       #149.16
        movsd     14848(%esp,%ecx), %xmm5                       #149.16
        lea       (%esi,%esi), %edi                             #149.12
        movsd     14856(%esp,%ecx), %xmm4                       #149.16
        movsd     15360(%esp,%ecx), %xmm3                       #149.16
        movsd     15368(%esp,%ecx), %xmm2                       #149.16
        movsd     15872(%esp,%ecx), %xmm1                       #149.16
        movsd     %xmm0, (%eax,%edi,8)                          #149.5
        movsd     8(%esp,%ecx), %xmm0                           #149.16
        movsd     %xmm0, 8(%eax,%edi,8)                         #149.5
        movsd     512(%esp,%ecx), %xmm0                         #149.16
        movsd     %xmm0, 16(%eax,%edi,8)                        #149.5
        movsd     520(%esp,%ecx), %xmm0                         #149.16
        movsd     %xmm0, 24(%eax,%edi,8)                        #149.5
        movsd     1024(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 32(%eax,%edi,8)                        #149.5
        movsd     1032(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 40(%eax,%edi,8)                        #149.5
        movsd     1536(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 48(%eax,%edi,8)                        #149.5
        movsd     1544(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 56(%eax,%edi,8)                        #149.5
        movsd     2048(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 64(%eax,%edi,8)                        #149.5
        movsd     2056(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 72(%eax,%edi,8)                        #149.5
        movsd     2560(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 80(%eax,%edi,8)                        #149.5
        movsd     2568(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 88(%eax,%edi,8)                        #149.5
        movsd     3072(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 96(%eax,%edi,8)                        #149.5
        movsd     3080(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 104(%eax,%edi,8)                       #149.5
        movsd     3584(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 112(%eax,%edi,8)                       #149.5
        movsd     3592(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 120(%eax,%edi,8)                       #149.5
        movsd     4096(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 128(%eax,%edi,8)                       #149.5
        movsd     4104(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 136(%eax,%edi,8)                       #149.5
        movsd     4608(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 144(%eax,%edi,8)                       #149.5
        movsd     4616(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 152(%eax,%edi,8)                       #149.5
        movsd     5120(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 160(%eax,%edi,8)                       #149.5
        movsd     5128(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 168(%eax,%edi,8)                       #149.5
        movsd     5632(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 176(%eax,%edi,8)                       #149.5
        movsd     5640(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 184(%eax,%edi,8)                       #149.5
        movsd     6144(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 192(%eax,%edi,8)                       #149.5
        movsd     6152(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 200(%eax,%edi,8)                       #149.5
        movsd     6656(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 208(%eax,%edi,8)                       #149.5
        movsd     6664(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 216(%eax,%edi,8)                       #149.5
        movsd     7168(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 224(%eax,%edi,8)                       #149.5
        movsd     7176(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 232(%eax,%edi,8)                       #149.5
        movsd     7680(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 240(%eax,%edi,8)                       #149.5
        movsd     7688(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 248(%eax,%edi,8)                       #149.5
        movsd     8192(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 256(%eax,%edi,8)                       #149.5
        movsd     8200(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 264(%eax,%edi,8)                       #149.5
        movsd     8704(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 272(%eax,%edi,8)                       #149.5
        movsd     8712(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 280(%eax,%edi,8)                       #149.5
        movsd     9216(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 288(%eax,%edi,8)                       #149.5
        movsd     9224(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 296(%eax,%edi,8)                       #149.5
        movsd     9728(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 304(%eax,%edi,8)                       #149.5
        movsd     9736(%esp,%ecx), %xmm0                        #149.16
        movsd     %xmm0, 312(%eax,%edi,8)                       #149.5
        movsd     10240(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 320(%eax,%edi,8)                       #149.5
        movsd     10248(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 328(%eax,%edi,8)                       #149.5
        movsd     10752(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 336(%eax,%edi,8)                       #149.5
        movsd     10760(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 344(%eax,%edi,8)                       #149.5
        movsd     11264(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 352(%eax,%edi,8)                       #149.5
        movsd     11272(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 360(%eax,%edi,8)                       #149.5
        movsd     11776(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 368(%eax,%edi,8)                       #149.5
        movsd     11784(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 376(%eax,%edi,8)                       #149.5
        movsd     12288(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 384(%eax,%edi,8)                       #149.5
        movsd     12296(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 392(%eax,%edi,8)                       #149.5
        movsd     12800(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 400(%eax,%edi,8)                       #149.5
        movsd     12808(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 408(%eax,%edi,8)                       #149.5
        movsd     13312(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 416(%eax,%edi,8)                       #149.5
        movsd     13320(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 424(%eax,%edi,8)                       #149.5
        movsd     13824(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 432(%eax,%edi,8)                       #149.5
        movsd     13832(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm0, 440(%eax,%edi,8)                       #149.5
        movsd     15880(%esp,%ecx), %xmm0                       #149.16
        movsd     %xmm7, 448(%eax,%edi,8)                       #149.5
        movsd     %xmm6, 456(%eax,%edi,8)                       #149.5
        movsd     %xmm5, 464(%eax,%edi,8)                       #149.5
        movsd     %xmm4, 472(%eax,%edi,8)                       #149.5
        movsd     %xmm3, 480(%eax,%edi,8)                       #149.5
        movsd     %xmm2, 488(%eax,%edi,8)                       #149.5
        movsd     %xmm1, 496(%eax,%edi,8)                       #149.5
        movsd     %xmm0, 504(%eax,%edi,8)                       #149.5
        movl      (%ebx), %edi                                  #151.8
        addl      %edi, %esi                                    #151.3
        addl      $16, %ecx                                     #147.23
        cmpl      $512, %ecx                                    #147.4
        jl        .B3.8         # Prob 96%                      #147.4
                                # LOE eax edx ecx ebx ebp esi edi
.B3.9:                          # Preds .B3.8
        movl      16908(%esp), %ebx                             #
        movl      16936(%esp), %eax                             #
        movl      %edi, 16912(%esp)                             #
        movl      (%eax), %edi                                  #139.16
        addl      $32, %ebp                                     #139.23
        lea       -32(%edi), %ecx                               #139.16
        cmpl      %ecx, %ebp                                    #139.2
        jle       .B3.5         # Prob 90%                      #139.2
                                # LOE eax edx ecx ebx ebp esi edi bl bh
.B3.10:                         # Preds .B3.9
        movl      16900(%esp), %eax                             #
        movl      %edi, 16896(%esp)                             #
        movl      16912(%esp), %edi                             #
        addl      $-32, %edi                                    #137.17
                                # LOE eax ecx ebx edi
.B3.11:                         # Preds .B3.10 .B3.3
        movl      %ebx, %edx                                    #154.2
        addl      $32, %eax                                     #137.24
        cmpl      %edi, %eax                                    #137.3
        jle       .B3.3         # Prob 90%                      #137.3
                                # LOE eax edx ecx ebx edi
.B3.13:                         # Preds .B3.11 .B3.1
        addl      $16916, %esp                                  #156.1
        popl      %ebx                                          #156.1
        popl      %ebp                                          #156.1
        popl      %esi                                          #156.1
        popl      %edi                                          #156.1
        ret                                                     #156.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	transp_,@function
	.size	transp_,.-transp_
	.data
# -- End  transp_
	.data
_2il0floatpacket.7:
	.long 0x9999999a,0x3fb99999	# xf64
	.type	_2il0floatpacket.7,@object
	.size	_2il0floatpacket.7,8
_2il0floatpacket.5:
	.long 0x00000000,0x3ff00000	# xf64
	.type	_2il0floatpacket.5,@object
	.size	_2il0floatpacket.5,8
_2il0floatpacket.3:
	.long 0x00000000,0x40400000	# xf64
	.type	_2il0floatpacket.3,@object
	.size	_2il0floatpacket.3,8
_2il0floatpacket.1:
	.long 0x00000000,0x409f4000	# xf64
	.type	_2il0floatpacket.1,@object
	.size	_2il0floatpacket.1,8
__STRING.4:
	.byte 37	# s8
	.byte 101	# s8
	.byte 32	# s8
	.byte 77	# s8
	.byte 98	# s8
	.byte 121	# s8
	.byte 101	# s8
	.byte 116	# s8
	.byte 101	# s8
	.byte 115	# s8
	.byte 47	# s8
	.byte 115	# s8
	.byte 101	# s8
	.byte 99	# s8
	.byte 32	# s8
	.byte 40	# s8
	.byte 49	# s8
	.byte 54	# s8
	.byte 43	# s8
	.byte 49	# s8
	.byte 54	# s8
	.byte 32	# s8
	.byte 98	# s8
	.byte 121	# s8
	.byte 116	# s8
	.byte 101	# s8
	.byte 115	# s8
	.byte 32	# s8
	.byte 114	# s8
	.byte 101	# s8
	.byte 97	# s8
	.byte 100	# s8
	.byte 43	# s8
	.byte 119	# s8
	.byte 114	# s8
	.byte 105	# s8
	.byte 116	# s8
	.byte 101	# s8
	.byte 32	# s8
	.byte 112	# s8
	.byte 101	# s8
	.byte 114	# s8
	.byte 32	# s8
	.byte 111	# s8
	.byte 110	# s8
	.byte 101	# s8
	.byte 32	# s8
	.byte 99	# s8
	.byte 111	# s8
	.byte 109	# s8
	.byte 112	# s8
	.byte 108	# s8
	.byte 101	# s8
	.byte 120	# s8
	.byte 41	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.4,@object
	.size	__STRING.4,57
	.space 3	# pad
__STRING.3:
	.byte 67	# s8
	.byte 76	# s8
	.byte 79	# s8
	.byte 67	# s8
	.byte 75	# s8
	.byte 58	# s8
	.byte 32	# s8
	.byte 37	# s8
	.byte 108	# s8
	.byte 108	# s8
	.byte 105	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.3,@object
	.size	__STRING.3,13
	.space 3	# pad
__STRING.2:
	.byte 79	# s8
	.byte 117	# s8
	.byte 116	# s8
	.byte 112	# s8
	.byte 117	# s8
	.byte 116	# s8
	.byte 58	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 44	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 44	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 44	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.2,@object
	.size	__STRING.2,20
__STRING.1:
	.byte 73	# s8
	.byte 110	# s8
	.byte 112	# s8
	.byte 117	# s8
	.byte 116	# s8
	.byte 32	# s8
	.byte 58	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 44	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 44	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 44	# s8
	.byte 37	# s8
	.byte 101	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.1,@object
	.size	__STRING.1,20
__STRING.0:
	.byte 115	# s8
	.byte 105	# s8
	.byte 122	# s8
	.byte 101	# s8
	.byte 111	# s8
	.byte 102	# s8
	.byte 40	# s8
	.byte 100	# s8
	.byte 111	# s8
	.byte 117	# s8
	.byte 98	# s8
	.byte 108	# s8
	.byte 101	# s8
	.byte 41	# s8
	.byte 61	# s8
	.byte 37	# s8
	.byte 105	# s8
	.byte 44	# s8
	.byte 32	# s8
	.byte 40	# s8
	.byte 114	# s8
	.byte 111	# s8
	.byte 119	# s8
	.byte 44	# s8
	.byte 99	# s8
	.byte 111	# s8
	.byte 108	# s8
	.byte 41	# s8
	.byte 61	# s8
	.byte 40	# s8
	.byte 37	# s8
	.byte 105	# s8
	.byte 44	# s8
	.byte 37	# s8
	.byte 105	# s8
	.byte 41	# s8
	.byte 10	# s8
	.byte 0	# s8
	.type	__STRING.0,@object
	.size	__STRING.0,38
	.data
	.text
# -- Begin  main
# mark_begin;
       .align    4,0x90
	.globl   main
main:
.B4.1:                          # Preds .B4.0
        pushl     %ebx                                          #164.13
        movl      %esp, %ebx                                    #164.13
        andl      $-8, %esp                                     #164.13
        pushl     %edi                                          #164.13
        pushl     %esi                                          #164.13
        pushl     %ebp                                          #164.13
        subl      $100, %esp                                    #164.13
        pushl     %eax                                          #164.13
        pushl     %eax                                          #164.13
        stmxcsr   (%esp)                                        #164.13
        popl      %eax                                          #164.13
        orl       $32768, %eax                                  #164.13
        pushl     %eax                                          #164.13
        ldmxcsr   (%esp)                                        #164.13
        popl      %eax                                          #164.13
        popl      %eax                                          #164.13
        movl      $128, %eax                                    #170.3
        movl      %eax, 40(%esp)                                #170.3
        movl      $16384, %edx                                  #171.3
        movl      %edx, 44(%esp)                                #171.3
        pushl     %edx                                          #173.73
        pushl     %eax                                          #173.73
        push      $8                                            #173.73
        push      $__STRING.0                                   #173.73
        call      printf                                        #173.73
                                # LOE
.B4.2:                          # Preds .B4.1
        push      $33554448                                     #174.32
        call      malloc                                        #174.32
                                # LOE eax
.B4.18:                         # Preds .B4.2
        movl      %eax, %ebp                                    #174.32
                                # LOE ebp
.B4.3:                          # Preds .B4.18
        push      $33554448                                     #175.32
        call      malloc                                        #175.32
                                # LOE eax ebp
.B4.19:                         # Preds .B4.3
        movl      %eax, 88(%esp)                                #175.32
        addl      $24, %esp                                     #175.32
                                # LOE eax ebp al ah
.B4.4:                          # Preds .B4.19
        movl      %eax, %ecx                                    #177.3
        movl      %ebp, 60(%esp)                                #179.7
        lea       8(%ebp), %esi                                 #176.3
        lea       8(%ecx), %edx                                 #177.3
        xorl      %edi, %edi                                    #179.7
        movl      %esi, 80(%esp)                                #176.3
        movl      %edx, 92(%esp)                                #177.3
        xorl      %edx, %edx                                    #179.7
        xorl      %esi, %esi                                    #179.7
        movl      $1, %ecx                                      #179.7
        movl      $2, %eax                                      #179.7
        movl      %eax, 68(%esp)                                #179.7
        movl      $3, %eax                                      #179.7
        movl      %eax, 72(%esp)                                #179.7
        movl      $4, %eax                                      #179.7
        movl      %eax, 76(%esp)                                #179.7
        movl      $-1, %eax                                     #179.7
        movl      %eax, 84(%esp)                                #179.7
        movl      $-2, %eax                                     #179.7
        movl      %eax, 88(%esp)                                #179.7
        movl      $-3, %eax                                     #179.7
        movl      %eax, 96(%esp)                                #179.7
        movl      $-4, %eax                                     #179.7
        movl      %eax, 56(%esp)                                #179.7
        .align    4,0x90
                                # LOE edx ecx esi edi
.B4.5:                          # Preds .B4.4 .B4.5
        cvtsi2sd  %edx, %xmm0                                   #180.22
        cvtsi2sd  %ecx, %xmm3                                   #180.22
        mulsd     _2il0floatpacket.7, %xmm0                     #180.22
        movl      60(%esp), %eax                                #180.2
        movl      84(%esp), %ebp                                #181.22
        cvtsi2sd  %ebp, %xmm7                                   #181.22
        movl      68(%esp), %ebp                                #180.22
        cvtsi2sd  %ebp, %xmm6                                   #180.22
        movl      88(%esp), %ebp                                #181.22
        cvtsi2sd  %ebp, %xmm5                                   #181.22
        movl      72(%esp), %ebp                                #180.22
        cvtsi2sd  %ebp, %xmm4                                   #180.22
        movl      96(%esp), %ebp                                #181.22
        cvtsi2sd  %ebp, %xmm2                                   #181.22
        movl      76(%esp), %ebp                                #180.22
        cvtsi2sd  %ebp, %xmm1                                   #180.22
        movl      56(%esp), %ebp                                #181.22
        movsd     %xmm0, 8(%edi,%eax)                           #180.2
        cvtsi2sd  %esi, %xmm0                                   #181.22
        mulsd     _2il0floatpacket.7, %xmm0                     #181.22
        movsd     %xmm0, 16(%edi,%eax)                          #181.2
        mulsd     _2il0floatpacket.7, %xmm3                     #180.22
        cvtsi2sd  %ebp, %xmm0                                   #181.22
        movsd     %xmm3, 24(%edi,%eax)                          #180.2
        mulsd     _2il0floatpacket.7, %xmm7                     #181.22
        movsd     %xmm7, 32(%edi,%eax)                          #181.2
        mulsd     _2il0floatpacket.7, %xmm6                     #180.22
        addl      $5, 68(%esp)                                  #179.21
        movsd     %xmm6, 40(%edi,%eax)                          #180.2
        mulsd     _2il0floatpacket.7, %xmm5                     #181.22
        movsd     %xmm5, 48(%edi,%eax)                          #181.2
        mulsd     _2il0floatpacket.7, %xmm4                     #180.22
        addl      $5, 72(%esp)                                  #179.21
        movsd     %xmm4, 56(%edi,%eax)                          #180.2
        mulsd     _2il0floatpacket.7, %xmm2                     #181.22
        movsd     %xmm2, 64(%edi,%eax)                          #181.2
        mulsd     _2il0floatpacket.7, %xmm1                     #180.22
        addl      $5, 76(%esp)                                  #179.21
        movsd     %xmm1, 72(%edi,%eax)                          #180.2
        mulsd     _2il0floatpacket.7, %xmm0                     #181.22
        movsd     %xmm0, 80(%edi,%eax)                          #181.2
        addl      $80, %edi                                     #179.21
        addl      $-5, %esi                                     #179.21
        addl      $-5, 84(%esp)                                 #179.21
        addl      $5, %ecx                                      #179.21
        addl      $5, %edx                                      #179.21
        addl      $-5, 88(%esp)                                 #179.21
        addl      $-5, 96(%esp)                                 #179.21
        addl      $-5, 56(%esp)                                 #179.21
        cmpl      $2097146, %edx                                #179.3
        jle       .B4.5         # Prob 97%                      #179.3
                                # LOE eax edx ecx esi edi al ah
.B4.6:                          # Preds .B4.5
        movl      %eax, %ebp                                    #
        lea       (%edx,%edx), %ecx                             #
        addl      %ecx, %ecx                                    #
        addl      %ecx, %ecx                                    #
        addl      %ecx, %ecx                                    #
        movl      %edx, %eax                                    #
        negl      %eax                                          #
        .align    4,0x90
                                # LOE eax edx ecx ebp
.B4.7:                          # Preds .B4.6 .B4.7
        cvtsi2sd  %edx, %xmm0                                   #180.22
        cvtsi2sd  %eax, %xmm1                                   #181.22
        mulsd     _2il0floatpacket.7, %xmm0                     #180.22
        movsd     %xmm0, 8(%ecx,%ebp)                           #180.2
        mulsd     _2il0floatpacket.7, %xmm1                     #181.22
        movsd     %xmm1, 16(%ecx,%ebp)                          #181.2
        addl      $16, %ecx                                     #179.21
        addl      $-1, %eax                                     #179.21
        addl      $1, %edx                                      #179.21
        cmpl      $2097152, %edx                                #179.3
        jl        .B4.7         # Prob 80%                      #179.3
                                # LOE eax edx ecx ebp
.B4.8:                          # Preds .B4.7
        call      get_rt                                        #184.8
                                # LOE eax ebp
.B4.21:                         # Preds .B4.8
        movl      %edx, 68(%esp)                                #184.8
        movl      %eax, 56(%esp)                                #184.8
                                # LOE ebp
.B4.9:                          # Preds .B4.21
        movl      92(%esp), %esi                                #185.28
        lea       40(%esp), %eax                                #185.11
        lea       44(%esp), %edx                                #185.17
        pushl     %esi                                          #185.28
        movl      84(%esp), %ecx                                #185.28
        pushl     %ecx                                          #185.28
        pushl     %edx                                          #185.28
        pushl     %eax                                          #185.28
        call      transp_                                       #185.28
                                # LOE ebp
.B4.22:                         # Preds .B4.9
        addl      $16, %esp                                     #185.28
                                # LOE ebp
.B4.10:                         # Preds .B4.22
        call      get_rt                                        #186.8
                                # LOE eax ebp
.B4.24:                         # Preds .B4.10
        movl      %edx, %esi                                    #186.8
        movl      %eax, %edi                                    #186.8
                                # LOE ebp esi edi
.B4.11:                         # Preds .B4.24
        movsd     8(%ebp), %xmm3                                #187.33
        movsd     16(%ebp), %xmm2                               #187.40
        movsd     24(%ebp), %xmm1                               #187.47
        movsd     32(%ebp), %xmm0                               #187.54
        movsd     %xmm3, 4(%esp)                                #187.54
        movsd     %xmm2, 12(%esp)                               #187.54
        movsd     %xmm1, 20(%esp)                               #187.54
        movsd     %xmm0, 28(%esp)                               #187.54
        movl      $__STRING.1, (%esp)                           #187.54
        call      printf                                        #187.54
                                # LOE esi edi
.B4.12:                         # Preds .B4.11
        movl      64(%esp), %ecx                                #188.33
        movsd     8(%ecx), %xmm2                                #188.33
        movsd     16(%ecx), %xmm1                               #188.40
        movsd     24(%ecx), %xmm0                               #188.47
        movsd     32(%ecx), %xmm4                               #188.54
        movsd     %xmm2, 4(%esp)                                #188.54
        movsd     %xmm1, 12(%esp)                               #188.54
        movsd     %xmm0, 20(%esp)                               #188.54
        movsd     %xmm4, 28(%esp)                               #188.54
        movl      $__STRING.2, (%esp)                           #188.54
        call      printf                                        #188.54
                                # LOE esi edi
.B4.13:                         # Preds .B4.12
        movl      56(%esp), %ebp                                #189.32
        movl      68(%esp), %ecx                                #189.32
        subl      %ebp, %edi                                    #189.32
        sbbl      %ecx, %esi                                    #189.32
        pushl     %esi                                          #189.32
        pushl     %edi                                          #189.32
        push      $__STRING.3                                   #189.32
        call      printf                                        #189.32
                                # LOE esi edi
.B4.25:                         # Preds .B4.13
        addl      $12, %esp                                     #189.32
                                # LOE esi edi
.B4.14:                         # Preds .B4.25
        fldl      _2il0floatpacket.9                            #190.3
        movl      $__STRING.4, (%esp)                           #191.71
        movl      %edi, 48(%esp)                                #190.33
        movl      %esi, 52(%esp)                                #190.33
        fildll    48(%esp)                                      #190.33
        fdivrp    %st, %st(1)                                   #190.33
        fstpl     4(%esp)                                       #191.71
        call      printf                                        #191.71
                                # LOE
.B4.15:                         # Preds .B4.14
        xorl      %eax, %eax                                    #192.1
        addl      $100, %esp                                    #192.1
        popl      %ebp                                          #192.1
        popl      %esi                                          #192.1
        popl      %edi                                          #192.1
        movl      %ebx, %esp                                    #192.1
        popl      %ebx                                          #192.1
        ret                                                     #192.1
        .align    4,0x90
                                # LOE
# mark_end;
	.type	main,@function
	.size	main,.-main
	.data
# -- End  main
	.data
	.space 6	# pad
_2il0floatpacket.9:
	.long 0x00000000,0x423f4000	# xf64
	.type	_2il0floatpacket.9,@object
	.size	_2il0floatpacket.9,8
	.data
# End
