.PHONY: clean all checkallvars checkvars nodtests nodtests-lazbuild
default: all

OSDETECT:=
ifeq ($(OS),Windows_NT)
	OSDETECT:=WIN32
else
	UNAME_S:=$(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		OSDETECT:=LINUX
	endif
	ifeq ($(UNAME_S),Darwin)
		OSDETECT:=OSX
	endif
endif

CPUDETECT:=
ifeq ($(OS),Windows_NT)
	ifeq ($(PROCESSOR_ARCHITECTURE),AMD64)
		CPUDETECT:=AMD64
	endif
	ifeq ($(PROCESSOR_ARCHITECTURE),x86)
		CPUDETECT:=IA32
	endif
else
	UNAME_P := $(shell uname -p)
	ifeq ($(UNAME_P),x86_64)
		CPUDETECT:=AMD64
	endif
	ifneq ($(filter %86,$(UNAME_P)),)
		CPUDETECT:=IA32
	endif
	ifneq ($(filter arm%,$(UNAME_P)),)
		CPUDETECT:=ARM
	endif
endif

PATHDELIM:=/
ifeq ($(OSDETECT),WIN32)
	PATHDELIM:=\\
endif
PATHDELIM:=$(strip $(PATHDELIM))

PCP:=
ifeq ($(OSDETECT),WIN32)
	PCP:=$(LOCALAPPDATA)\lazarus
else
	ifeq ($(OSDETECT),LINUX)
		PCP:='~/.lazarus'
	else
		ifeq ($(OSDETECT),OSX)
			PCP:=~/.lazarus
		else
			PCP:=~/.lazarus
		endif
	endif
endif

LP:=
ifeq ($(OSDETECT),WIN32)
	LP:=C:\lazarus
else
	ifeq ($(OSDETECT),LINUX)
		LP:=~/lazarus
	else
		ifeq ($(OSDETECT),OSX)
			LP:=~/lazarus
		else
			LP:=~/lazarus
		endif
	endif
endif

LAZBUILD:=$(LP)$(PATHDELIM)lazbuild

checkallvars: checkvars 
	@echo OSDETECT=$(OSDETECT)
	@echo CPUDETECT=$(CPUDETECT)

checkvars:              
	@echo PCP=$(PCP)
	@echo LP=$(LP)

clean:                  
	rm -rf lib/*
	rm -rf lib

all: checkvars
	$(LAZBUILD) --pcp=$(PCP) testzengine.lpr

# Тесты NOD (cad_source/zengine/TZ_NOD_NamedObjectDictionary.md, этап 8).
# nodtests - сборка консольным fpc без IDE (как в CI), см. nodtests.sh;
# выбрать тесты: make nodtests NODTESTS="nodstage5 nodstage7"
NODTESTS:=$(basename $(wildcard nodstage*.lpr))

nodtests:
	bash nodtests.sh $(NODTESTS)

# nodtests-lazbuild - сборка проектов nodstage*.lpi через lazbuild
# (пакеты zengine должны быть установлены в $(PCP)) и запуск
nodtests-lazbuild: checkvars
	$(foreach T,$(NODTESTS),$(LAZBUILD) --pcp=$(PCP) $(T).lpi &&) true
	$(foreach T,$(NODTESTS),.$(PATHDELIM)$(T) ..$(PATHDELIM)..$(PATHDELIM).. &&) true
