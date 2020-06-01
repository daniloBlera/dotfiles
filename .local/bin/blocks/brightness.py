#!/usr/bin/env python3
# -*- coding: utf-8 -*-
# Brightness script for dwmblocks
# Displaying values in 5% increments due to variations on xbacklight output
import os


def main():
    brightness = float(os.popen('xbacklight').read())

    if brightness < 5:
        brightness = round(brightness)
    else:
        brightness = round(brightness / 5) * 5  # rounding to 5%

    print(f' BR {brightness}% \n ')


if __name__ == '__main__':
    main()
