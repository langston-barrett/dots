REMOTE := cfern:/home/siddharthist
RSYNC := rsync \
	     -e ssh \
	     --human-readable \
	     --progress \
	     --archive \
		 --exclude=syncthing \
		 --exclude=result \
	     --exclude=log \
		 --include='.*'

result/%.tar.gz: %.nix
	nix-build $<

%.image: result/%.tar.gz
	sudo docker load < result
	touch $@

.PHONY: to
to:
	$(RSYNC) \
	  . \
	  $(REMOTE)

.PHONY: from
from:
	$(RSYNC) $(REMOTE) .

.PHONY: run
run: syncthing.image
	sudo docker-compose up |& tee log

.PHONY: clean
clean:
	sudo docker-compose rm --stop
