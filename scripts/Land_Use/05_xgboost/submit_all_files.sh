#!/bin/bash

sh mail.sh 2_xgboost_allbands &

sh mail.sh 3_xbgoost_ndvi &

sh mail.sh 4_xbgoost_ndvi_smoothed &

sh mail.sh 5_xbgoost_two_classes_ndvi &

sh mail.sh 6_xbgoost_two_classes_ndvi_smoothed &

#End of Script