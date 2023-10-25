all.wasm: all.wat
	wat2wasm all.wat

all.wat:
	dune build
	dune exec wasmpy_compiler > all.wat

clean:
	rm all.*