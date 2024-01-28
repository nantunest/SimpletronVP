pwmtest: pwmtest.hs
	ghc -o pwmtest pwmtest.hs

clean:
	rm -rf *.hi
	rm -rf *.o

clean_%: $(OBJS)
	@echo $@
	rm $(subst clean_,,$@)

%: $(OBJS)
	@echo $@
	ghc -main-is $@ -o $@ $@.hs 
	./$@