# eVenti — venti in Erlang

The eVenti project implements an experimental Erlang-based venti server. The concept of venti originates from the operating system Plan 9. Venti provides an archival storage which is content adressed, idempotent and where data can never be deleted. It is perfect for storing backups and other archival data sets.

Our implementation leverages LevelDB through *eleveldb* and *ranch* for the acceptor pool.

The implementation has been tested with the plan9port tool set. And with a go client as well. While it appears to work correctly, there is really no guarantee right now. The code base is very small though, so chances are that eventual errors are in the LevelDB backing store.

