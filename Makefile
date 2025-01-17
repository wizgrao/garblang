.PHONY: clean
build:
	cargo build --release
install: build
	mkdir -p ${GARBROOT}/bin
	mkdir -p ${GARBROOT}/lib
	cp target/release/garblang ${GARBROOT}/bin
	cp lib.garb ${GARBROOT}/lib
