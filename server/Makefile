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

.PHONY: to
to:
	$(RSYNC) \
	  . \
	  $(REMOTE)

.PHONY: from
from:
	$(RSYNC) --exclude=code $(REMOTE) .

.PHONY: run
run: syncthing.image
	sudo docker-compose up |& tee log

.PHONY: clean
clean:
	sudo docker-compose rm --stop
