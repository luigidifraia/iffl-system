# Disk images

## CMD FD2000 disk images
In order to use the provided disk images for CMD FD2000 floppy drives with the [Versatile Commodore Emulator](http://vice-emu.sourceforge.net), VICE, you need to acquire an FD2000 ROM first.

### Using the native partition
You can copy files from either a .d64 or .d81 image to a .d2m one that uses the native partition by using a file copier, e.g. IDE64's File Manager, which is part of [IDEDOS](http://idedos.ide64.org).\
Setup instructions for IDEDOS in VICE are available [here](http://www.ide64.org/vice_ide64_howto_rev2.txt). The IDE64 File Manager can be started with the F8 key once you are in direct mode. For additional commands, including file copy and delete, have a look at the IDE64 user's guide [here](http://www.ide64.org/down.html).

### Using 1541/1581 partitions
The images that use 1541/1581 partitions leverage CMD FD2000's emulation capabilities and can be put together without native tools:
- For `fd2000_new_1541_partition.d2m` just overwrite the first 0x2AB00 bytes with the contents of a .d64 image whose files you want to copy over, leaving out error data.
- For `fd2000_new_1581_partition.d2m` just overwrite the first 0xC8000 bytes with the contents of a .d81 image whose files you want to copy over.