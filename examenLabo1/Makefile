file = wedding

$(file): $(file).pl
	swipl -O -q -g main --stand_alone=true -o $(file) -c $(file).pl

.PHONY: clean
clean:
	-@rm -f clauses header infile.cnf model
