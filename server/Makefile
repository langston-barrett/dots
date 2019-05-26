result/%.tar.gz: %.nix
	nix-build $<

%.image: result/%.tar.gz
	sudo docker load < result
	touch $@

.PHONY: copy
copy: syncthing.nix
	rsync \
	  --human-readable \
	  --progress \
	  --archive \
	  --delete \
	  --delete-excluded \
	  --exclude=syncthing \
	  --exclude=result \
	  --exclude=log \
	  -e ssh \
	  . \
	  cfern:/home/siddharthist

.PHONY: remote
remote: copy
	ssh cfern 'make syncthing.image'

.PHONY: run
run: syncthing.image
	sudo docker-compose up |& tee log

.PHONY: clean
clean:
	sudo docker-compose rm --stop
