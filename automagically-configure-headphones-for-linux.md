# Overview

This is for bluetooth-enabled headphone devices

# Connect

```
$ bluetoothctl
$ power on
$ agent on
$ default-agent
$ scan on
```

Make sure your headphones are in pairing mode. They should automatically connect.

If they do not automatically connect, then attempt the following

```
$ pair FC:F1:52:DA:5C:50
```

Wait for a success pairing and connect

```
$ connect FC:F1:52:DA:5C:50
```

Wait for a success

```
$ scan off
$ exit
```

# Enable Auto Connect



