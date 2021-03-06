---
layout: post
title: Amazon SQS vs. RabbitMQ
categories:
- Distributed Systems
tags: []
status: publish
type: post
published: true
meta:
  _wpas_done_linkedin: '1'
  publicize_results: a:1:{s:7:"twitter";a:1:{i:11342442;a:2:{s:7:"user_id";s:11:"davekincaid";s:7:"post_id";s:18:"147488737222336514";}}}
  _wpas_done_twitter: '1'
  _elasticsearch_indexed_on: '2011-12-16 01:30:29'
---

Recently at work we've been working on selecting a message queue system for a new project we're
working on. The three that we looked at were <a href="http://activemq.apache.org"
target="_blank">ActiveMQ</a>, <a href="http://aws.amazon.com/sqs/" target="_blank">Amazon SQS</a>
and <a href="http://www.rabbitmq.com/" target="_blank">RabbitMQ</a>. We'll be running the
applications on Amazon's EC2, so SQS seemed like just the thing. Unfortunately, it performs so
poorly that it wasn't even a contest in the end.

We threw out ActiveMQ pretty early before even running it through the performance tests due to the
horrible clustering support. It seems to be geared more toward load balancing instead of high
availability and fast failure recovery. It's
described <a href="http://activemq.apache.org/masterslave.html" target="_blank">here</a>. We really
didn't think it met our HA requirements.

So I set out to do a performance test between Amazon SQS and RabbitMQ to see if they were close. I
was really hoping that SQS would work since the setup and maintenance time would be next to
nothing. We setup two EC2 small instances and installed RabbitMQ on one. The other would be used to
run the Java program that ran the tests.

The idea was that when the program started up it would create 2 threads. One thread would publish
10,000 messages to the queue as fast as it could one after the other. The other thread would consume
the 10,000 message from the queue one after the other as fast as it could. The results were
astonishing. Running against SQS took a little over 6 minutes and RabbitMQ took 12 seconds. I
suspected that RabbitMQ would be faster, but not that much faster. Our decision was easy.

I realize this isn't a scientific benchmark, but the vast difference in results speaks for itself
and a carefully constructed experiment isn't needed. I uploaded the code I used to Github:
https://github.com/dkincaid/MQBenchmark in case you're interested in trying it out. Please
understand this was a very quick program thrown together quickly for a single purpose. I fully
expected to throw it away when I was done.
