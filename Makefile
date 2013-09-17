PROJECT = transeo

ERLC_OPTS = +debug_info +'{parse_transform, lager_transform}'

DEPS = lager ranch gproc

dep_lager = https://github.com/basho/lager.git 2.0.0
dep_ranch = pkg://ranch 0.8.5
dep_gproc = https://github.com/esl/gproc.git 6f7f6c32e1a26c0df8215248a54594a05401b296

.PHONY: release clean-release console

release: clean-release app
	relx -o rel/$(PROJECT)

clean-release:
	rm -rf rel/$(PROJECT)

console: release
	./rel/$(PROJECT)/bin/$(PROJECT) console -pa ../../deps/*/ebin

include erlang.mk
