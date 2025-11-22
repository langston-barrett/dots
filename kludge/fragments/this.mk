# All rules should depend on the Makefile itself
THIS = $(abspath $(lastword $(MAKEFILE_LIST)))
