#!/bin/bash

pacmd set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo $(printf '0x%x' $(( $(pacmd dump|grep analog-stereo|grep set-sink-volume|head -n 1|cut -f3 -d' ') - 0xf00)) )
