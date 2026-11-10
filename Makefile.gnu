F90 = gfortran
F77 = gfortran
CC  = gcc
OPT = -O3 -std=legacy
COPT = -O3
BLAS = -lblas
LIB = -lm
F90OPT = $(OPT)

CDIR = ftest

TARGETS = \
     nd1  nd2  nd3  nd4  nd5  nd6  nd7  nd8  nd9 \
nd10      nd12           nd15 nd16      nd18 \
nd20 nd21 nd22 nd23 nd24 nd25


# These programs need BLAS.
TARGETS_BLAS = nd11 nd8_blas nd20_blas

TARGETS_ASM = nd8_asm nd20_asm

TARGETS_LATEST=nd2 nd20 nd24 nd25 ndfftw

.f.o:
	$(F77) $(OPT) -c $<
.c.o:
	$(CC) $(COPT) -c $<

desc:
	@echo "\n\n"
	@echo type \'make test0\' OR \'make test1\' then,  "\n"
	@echo type \'make nd?  [nd? [nd? [ ... ]]]\'  OR \'make all\' "\n""\n"

C_NAME:
	sh c_name.sh
	sh optu.sh
	touch C_NAME

usrtime.c fftnd_fftw.c : C_NAME

clean:
	\rm -f *.o nd[0-9] nd[0-9][0-9] nd*_asm nd*_blas nd*_asm.f nd*_blas.f C_NAME

rclean: 
	\rm -f *.o nd[0-9] nd[0-9][0-9] nd*_asm nd*_blas nd*.f

TAR_CONT = ${CDIR}/*.f ${CDIR}/*.c ${CDIR}/Makefile \
${CDIR}/*.txt ${CDIR}/*.tex ${CDIR}/*.sh ${CDIR}/go* ${CDIR}/*.pl \
${CDIR}/patch

tar:
	( cd .. ; tar cvf - $(TAR_CONT) | \
	gzip -9 > nd_fft.tar.gz)

all: $(TARGETS) $(TARGETS_BLAS) $(TARGETS_ASM)

noblas: $(TARGETS)

latest: $(TARGETS_LATEST)

test0:
		cp -f ffttest_0.f ffttest.f
test1:
		cp -f ffttest_1.f ffttest.f
test2:
		cp -f ffttest_2.f ffttest.f
test3:
		cp -f ffttest_3.f ffttest.f
test4:
		cp -f ffttest_4.f ffttest.f

BASE = fftprep2.f fftprep3.f fftprep4_dummy.f fftipost.f
ndbase.f: $(BASE)
	cat $(BASE) > ndbase.f
OBASE= ffttest.o ndbase.o usrtime.o

ndfftw: fftnd_fftw.o $(OBASE) libfftw.a
	$(F77) $(OPT) -o $@ fftnd_fftw.o $(OBASE) libfftw.a -lm
ndasl: fftnd_asl.o transp_1.o $(OBASE)
	$(F77) $(OPT) -o $@ fftnd_asl.o transp_1.o $(OBASE) -lasl
ndmfft: fftnd_mfft.o $(OBASE) libmfft.a 
	$(F77) $(OPT) -o $@ fftnd_mfft.o $(OBASE) libmfft.a
ndgpfa: fftnd_gpfa.o $(OBASE) libgpfa.a
	$(F77) $(OPT) -o $@ fftnd_gpfa.o $(OBASE) libgpfa.a
ndgpfa2: fftnd_gpfa2.o transp_1.o $(OBASE) libgpfa.a
	$(F77) $(OPT) -o $@ fftnd_gpfa2.o transp_1.o $(OBASE) libgpfa.a
ndgpfas: gpfa_stride.o ndbase.o usrtime.o libgpfa.a
	$(F77) $(OPT) -o $@ gpfa_stride.o ndbase.o usrtime.o libgpfa.a
ndssl: fftnd_ssl.o $(OBASE)
	$(F77) $(OPT) -o $@ fftnd_ssl.o $(OBASE) -lssl2vp
ndsslx: fftnd_sslx.o usrtime.o
	$(F77) $(OPT) -o $@ fftnd_sslx.o usrtime.o -lssl2vp
nd20x: fftnd_20x.o usrtime.o
	$(F77) $(OPT) -o $@ fftnd_20x.o usrtime.o

$(TARGETS): % : ffttest.o usrtime.o %.f
	$(F77) $(OPT) -o $@ $(SRC.$@:.f=.o) ffttest.o usrtime.o $(LIB)

$(TARGETS_BLAS): % : ffttest.o usrtime.o %.f
	$(F77) $(OPT) -o $@ $(SRC.$@:.f=.o) ffttest.o usrtime.o $(LIB) $(BLAS)

$(TARGETS_ASM): % : ffttest.o usrtime.o transp_1.o %.f
	$(F77) $(OPT) -o $@ $(SRC.$@:.f=.o) transp_1.o ffttest.o usrtime.o $(LIB)

TYPE0=fftprep1_0.f  fftprep2.f fftprep3.f fftprep4_dummy.f fftipost.f
TYPE1=fftprep1_1.f  fftprep2.f fftprep3.f fftprep4_dummy.f fftipost.f
TYPE2=fftprep1_0.f  fftprep2.f fftprep3.f fftprep4.f       fftipost.f

# Rotate Left
SRC.nd1 =fftnd_v0.f rearr_0.f $(TYPE0)
SRC.nd2 =fftnd_v0.f rearr_1.f $(TYPE1)
SRC.nd6 =fftnd_v1.f $(TYPE1)

nd1.f: $(SRC.nd1:.f=.o)
	cat $(SRC.nd1) > $@
nd2.f: $(SRC.nd2:.f=.o)
	cat $(SRC.nd2) > $@
nd6.f: $(SRC.nd6:.f=.o)
	cat $(SRC.nd6) > $@

# Rotate Left Real Version
SRC.nd3 =fftnd_vr1.f rearr_1.f $(TYPE1)
nd3.f: $(SRC.nd3:.f=.o)
	cat $(SRC.nd3) > $@

# calling multiple FFT + small work space
#SRC.nd4=fftnd_smf1.f mfft_1.f $(TYPE1)
SRC.nd4=fftnd_smf1.f mfft_2.f \
decn1m_1.f decn2m_1.f decn3m_1.f decx1m_1.f decx2m_1.f decx2fm_1.f $(TYPE1)
nd4.f: $(SRC.nd4:.f=.o)
	cat $(SRC.nd4) > $@

# calling multiple FFT without bit-reversal + optimized work space
SRC.nd5=fftnd_smfnbw2.f mfftnb_3.f btrv_2.f btrvm_1.f \
dec5fm_1.f dec4fm_1.f dec3fm_1.f dec2fm_1.f dec1fm_1.f dec4m_1.f $(TYPE2)
SRC.nd7=fftnd_smfnbw2.f mfftnb_2.f btrv_2.f btrvm_1.f \
dec3m_1.f dec2m_1.f dec1m_1.f dec3fm_1.f dec4m_1.f $(TYPE2)
SRC.nd10=fftnd_smfnbw2.f mfftnb_2sr.f btrv_2.f btrvm_1.f \
dec3m_1.f dec2m_1.f dec1m_1.f dec3fm_1.f $(TYPE2)
nd5.f: $(SRC.nd5:.f=.o)
	cat $(SRC.nd5) > $@
nd7.f: $(SRC.nd7:.f=.o)
	cat $(SRC.nd7) > $@
nd10.f: $(SRC.nd10:.f=.o)
	cat $(SRC.nd10) > $@

# multiple FFT + transpose
SRC.nd8 =fftnd_vtmf1.f  mfft_1.f transp_1.f $(TYPE1)
SRC.nd9 =fftnd_vtmf1.f  mfft_2.f transp_1.f \
decn1m_1.f decn2m_1.f decn3m_1.f decx1m_1.f decx2m_1.f decx2fm_1.f $(TYPE1)
nd8.f: $(SRC.nd8:.f=.o)
	cat $(SRC.nd8) > $@
nd9.f: $(SRC.nd9:.f=.o)
	cat $(SRC.nd9) > $@

# multiple call of FFT
SRC.nd11=fftnd_stf1.f fft_2.f transp_blas.f \
decn1_1.f decn2_1.f decn3_1.f decx1_1.f decx2_1.f decx2f_1.f $(TYPE0)
nd11.f: $(SRC.nd11:.f=.o)
	cat $(SRC.nd11) > $@

# multiple call of FFT without bit reversal
SRC.nd12=fftnd_sfnb1.f fftnb_1.f $(TYPE2) \
dec3fm_1.f dec4m_1.f dec3l_1.f dec2l_1.f dec1l_1.f
nd12.f: $(SRC.nd12:.f=.o)
	cat $(SRC.nd12) > $@

# calling multiple FFT without bit reversal + using small work space
SRC.nd15=fftnd_smfnb1.f mfftnb_1.f $(TYPE2)
SRC.nd24=fftnd_smfnb1.f mfftnb_2.f \
dec3m_1.f dec2m_1.f dec1m_1.f dec3fm_1.f dec4m_1.f $(TYPE2)
SRC.nd23=fftnd_smfnb2.f mfftnb_2.f fftnb_1.f \
dec3m_1.f dec2m_1.f dec1m_1.f dec3fm_1.f dec4m_1.f \
dec3l_1.f dec2l_1.f dec1l_1.f $(TYPE2)
SRC.nd25=fftnd_smfnb3.f mfftnb_2.f \
dec3m_1.f dec2m_1.f dec1m_1.f dec3fm_1.f dec4m_1.f $(TYPE2)
nd15.f: $(SRC.nd15:.f=.o)
	cat $(SRC.nd15) > $@
nd23.f: $(SRC.nd23:.f=.o)
	cat $(SRC.nd23) > $@
nd24.f: $(SRC.nd24:.f=.o)
	cat $(SRC.nd24) > $@
nd25.f: $(SRC.nd25:.f=.o)
	cat $(SRC.nd25) > $@

# calling multiple FFT without bit reversal + transpose
SRC.nd16=fftnd_vtbmfnb1.f mfftnb_1.f transb_2.f $(TYPE2)
SRC.nd21=fftnd_vtmfnb1.f  mfftnb_1.f transp_1.f $(TYPE2)
#SRC.nd21=fftnd_vtmfnb1.f mfftnb_2.f transp_1.f \
#dec3m_1.f dec2m_1.f dec1m_1.f dec3fm_1.f dec4m_1.f $(TYPE2)
nd16.f: $(SRC.nd16:.f=.o)
	cat $(SRC.nd16) > $@
nd21.f: $(SRC.nd21:.f=.o)
	cat $(SRC.nd21) > $@

# calling multiple FFT without bit reversal + unrolling outer loop
SRC.nd22=fftnd_svmfnb1.f mfftnb_1.f $(TYPE2)
nd22.f: $(SRC.nd22:.f=.o)
	cat $(SRC.nd22) > $@

# calling multiple FFT + bitreversal without bitrev
SRC.nd18=fftnd_vtmfw1.f mfftw_1.f transp_1.f $(TYPE1)
SRC.nd20=fftnd_vtmfw1.f mfftw_2.f transp_1.f \
decb3m_1.f decb2m_1.f decb1m_1.f decb3fm_1.f $(TYPE1)
nd18.f: $(SRC.nd18:.f=.o)
	cat $(SRC.nd18) > $@
nd20.f: $(SRC.nd20:.f=.o)
	cat $(SRC.nd20) > $@

SRC.nd8_asm=fftnd_vtmf1.f  mfft_1.f $(TYPE1)
nd8_asm.f: $(SRC.nd8_asm:.f=.o)
	cat $(SRC.nd8_asm) > $@

SRC.nd20_asm=fftnd_vtmfw1.f mfftw_2.f \
decb3m_1.f decb2m_1.f decb1m_1.f decb3fm_1.f $(TYPE1)
nd20_asm.f: $(SRC.nd20_asm:.f=.o)
	cat $(SRC.nd20_asm) > $@

SRC.nd8_blas=fftnd_vtmf1.f  mfft_1.f transp_blas.f $(TYPE1)
nd8_blas.f: $(SRC.nd8_blas:.f=.o)
	cat $(SRC.nd8_blas) > $@

SRC.nd20_blas=fftnd_vtmfw1.f mfftw_2.f transp_blas.f \
decb3m_1.f decb2m_1.f decb1m_1.f decb3fm_1.f $(TYPE1)
nd20_blas.f: $(SRC.nd20_blas:.f=.o)
	cat $(SRC.nd20_blas) > $@

ARCH ?= $(shell uname -m)
ifeq ($(ARCH),x86_64)
TRANSP_ASM = transp1_x86_64.s
else
TRANSP_ASM = transp1.s
endif

transp_1.o: $(TRANSP_ASM)
	as -o transp_1.o $(TRANSP_ASM)

################ MPI ################
MPI_OBJ = ffttest_mpi.o fftnd_mpi_mfftnb_2.o
PARENT_FFT_OBJ = $(SRC.nd7:.f=.o)

$(MPI_OBJ):
	mpif90 -c  $(F90OPT) $*.f

para: $(MPI_OBJ) $(PARENT_FFT_OBJ) usrtime.o
	mpif90 -o para $(F90OPT) $(MPI_OBJ) $(PARENT_FFT_OBJ) usrtime.o
