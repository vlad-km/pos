# POS - lisp persistence objects storage  

Package **POS** - lisp ([JSCL][jscl]) persistence objects.

It is intended for use in the [Moren][moren] environment.

## Status

`Development`

## Use

- create - create/open object
- bind - bind object instance and variable
- save - save object to local-storage or file-system


Create local-storage object with name "Some" and default template = {}

```lisp
(let ((dbo (pos:create "Some" :storage :ls))
       (instance))
    (pos:bind instance dbo)
    (jso:_set (instance "name") "Jaan")
    (jso:_set (instance "profile") nil)
    (jso:_set (instance "kpi") (jso:mk "k1" 0.11 "k2" 1.1 "k3" -0.1))
    (pos:save dbo)))
```

Create object which will be expired 2019/04/12 at 00:00:00

```lisp
(pos:create "name" :storage :ls :default template-1
           :expire (expire-date :year 2019 :month 3 :day 12)
```


## Dependencies

Lisp packages: :json-file :jso



## Copyright
Copyright © 2017,2018 Vladimir Mezentsev

## License
GNU General Public License v3.0


[jscl]: <https://github.com/jscl-project/jscl>
[moren]: <https://github.com/vlad-km/moren-electron>
