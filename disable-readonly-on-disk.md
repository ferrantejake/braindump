# Disable Readonly on Disk

If you ever freeze your USB or disk into a read-only state, there is a fix for that

## Windows

In a terminal, open DiskPart

```
$ diskpart
```

Follow by listing your devices

```
$ list disk
```

Find your disk in question and run the following commands

```
$ select disk <#>
$ attributes clear disk readonly
```

And just like that you should be able to read/write to your disk now.

