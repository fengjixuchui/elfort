ELFORT=out/elfort


.PHONY: disasm
disasm:
	gcc -c out/check.s -o out/check.o && ld out/check.o -o out/check.out && objdump -d out/check.out


.PHONY: run
run: out $(ELFORT)
	$(ELFORT)


.PHONY: check-elf
check-elf: $(ELFORT)
	readelf -a $(ELFORT)


.PHONY: out
out:
	mkdir -p out


$(ELFORT): meta.f core.f
	arkam meta.f
	chmod +x $(ELFORT)


.PHONY: clean
clean:
	rm -f out/*
