# Demo 2

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->


- [Initial Setup](#initial-setup)
- [Setup](#setup)
- [Introduction](#introduction)
- [Part 1](#part-1)
- [Part 2](#part-2)
- [Part 3](#part-3)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Initial Setup

> Perform once on new cluster.

```bash
ansible-playbook -D -vv -i inventory.yml playbook.yml
```

## Setup

> Perform before demo.

* `screen -c .screenrc`
* Wipe database and restart services

  ```bash
  ansible-playbook -D -vv -i inventory.yml -l db -t cleanup playbook.yml && ansible-playbook -D -vv -i inventory.yml -l api,workers -t build,deploy -e restart=true playbook.yml
  ```
* `screen -c .screenrc.pis`

## Introduction

* Show raspberry pi htop dashboard
* Show application logs
* Show locust script
* Show locust interface

## Part 1

* Start with 10 users
* Show ongoing game
* Create and play game
* Show locust stats & graphs
* Show (hopefully empty) application logs

## Part 2

* Increase to 1000 users
* Show locust stats & graphs
* Show (hopefully empty) application logs
* Increase until it crashes
* Show errors in application logs

## Part 3

* Restart locust (screen 0)
* Restart demo (screen 3)
* Perform demo again up to 100 users
* Distribute
  * `Node.list()`
  * `Node.connect :'boardr@10.0.1.203'`
  * `Node.connect :'boardr@10.0.1.204'`
  * `Node.connect :'boardr@10.0.1.205'`
  * `Node.connect :'boardr@10.0.1.206'`
* Increase until it crashes
* Show distribution code
  * Swarm configuration
  * Game server swarm integration
  * Task distribution