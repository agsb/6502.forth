# use:
# to make: 		MY=file make 
# to clean: 	MY=file make clean

.PHONY: clean all

# MY = $(ARGV[1])

########################################

LD = cl65

LDFLAGS = --cpu 6502 --memory-model near \
	--no-target-lib --target none \
	--debug --debug-info

########################################


%: %.s

$(MY): $(SOURCES:.s=.o)

	$(LD) $(LDFLAGS) -C $@.cfg -Ln $@.lbl \
	-l $@.lst -m $@.map -o $@ $@.s \
	2> $@.err | tee $@.out

	cp $@.s $@.asm

	sort -k 2 < $@.lbl > $@.lbs

	od --endian=big -A x -t x1z -v $@ > $@.hex

all: 
	
	$(MY)

clean:
	
	$(RM) $(MY) $(MY).out $(MY).err $(MY).map $(MY).lst \
			    $(MY).lbl $(MY).lbs $(MY).hex


