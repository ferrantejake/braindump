# Test Writing Guidelines

The purpose of this article is to talk about different types of testing and which forms of testing are appropriate for each component of the Droplit system. We will do this by first differentiating _unit testing_ and _integration testing_, and then relating each to the different components of the Droplit system.

Disclaimer: we do not discuss validation/system tests in this article. These address a scope which our tests do not cover at the moment.

## Table of Contents

* [Unit Tests and Integration Tests: What Is The Difference?](#unit-tests-and-integration-tests-what-is-the-difference)
* [Guidelines and Best Practices for Writing Tests](#guidelines-and-best-practices-for-writing-tests)
* [System Components \(a non-comprehensive list\)](#system-components-a-non-comprehensive-list)

## Unit Tests and Integration Tests: What Is The Difference?

### **Unit Tests**

#### Definition

In computer programming, unit testing is a software testing method by which individual units of source code, sets of one or more computer program modules together with associated control data, usage procedures, and operating procedures, are tested to determine whether they are fit for use. Intuitively, one can view a unit as the smallest testable part of an application.

_The smallest possible test to determine if a component is working._

#### Goals

The purpose of unit tests is to test the core components of the system to ensure they are working so that other components may build on them. **An important note here**: unit tests should test that a component is working given the conditions a component expects. _We only care to test that a component is working with well-formatted data_. This means we can make assumptions about how the data _should_ be formatted, i.e. we can assume the data is formatted to the specification of the library in question.

#### Target Audience

Unit tests pertain to the lowest level of components of the system. For Droplit, this means tools like dq, ipc, redis-lock, among others. These libraries are those which other libraries in the system are dependent on, e.g. the api/routes infrastructure is dependent on dq, ipc, and redis-lock - just to name a few. These libraries - dq, ipc, redis-lock - are the libraries that undergo unit testing as they are the foundation for other components.

### **Integration Tests**

#### Definition

Integration testing \(sometimes called integration and testing, abbreviated I&T\) is the phase in software testing in which individual software modules are combined and tested as a group. It occurs after unit testing and before validation testing.

_Testing system components together in an established workflow as to determine if a workflow, and the workflow components therein, works as expected._

#### Goals

Integration tests ensure a system of components is working. A robust set of integration tests observe all the different ways the components of a system can interact. This means including tests with different possible states the system can be in and data that can be passed through the system. That being said, integration tests should only be written to observe how the current system works with expected data structures. Testing for edge cases should be left to manual testing.

#### Target Audience

Integration tests address systems of components working testing. For Droplit, this is systems such as the api interface. Two \(but definitely not all possible\) systems which come to mind are sending commands to edge devices and updating records via the api. Both of these systems start at the api. Sending commands to edge devices then makes its way through the device controller, conduit factory, transport layer, etc. all the way down to the hub. These components together make up the system which an integration tests would evaluate.

### **tl;dr**

Unit tests ensure  core components are working as expected. Integration tests make sure systems of components are working in a cohesive manner.

## Guidelines and Best Practices for Writing Tests

### **Unit Tests**

#### Do

1. **Use static preconditions**. When it comes to unit tests, we should assume that incoming parameters meet the structure requirements set forth by the component. Here, our goal is to test the component, not the component given unexpected conditions.
2. **Tests should be mutually exclusive**.  Tests should be written such that any test does not interfere with, build upon, or combine with another test. This leads to:
3. **State independent**. Because tests are mutually exclusive, they should not depend on the state/results of another test. This can lead to dependencies, in which case if changes are made to a test which other tests are dependent on, then all other tests must be re-assessed.
4. **Tests should be fast**. Typically, unit tests should be sub 500ms/each.

#### Do Not

1. **Stress tests the system**. The point of unit tests is to test if components are working in expected conditions in a typical scenario. Different test should be written for stress tests. i.e. Do not unnecessarily create 500 device records because you can. 2-3 should be plenty \(see _\#4 tests should be fast_\).
2. **Integrate other systems into a test**. Unit tests are specific to one component. They should not be dependent on the functioning of another component.

### **Integration Tests**

#### Do

1. **Thorough case coverage**. These systems are typically working under many different conditions. Integration tests should come as close to testing all possible scenarios, at least to what is reasonable.
2. **Start tests with static preconditions**. Tests should be expected to be tested in certain states. You will probably need to run the test several times to enumerate possible states.

#### Do Not

1. **Test every component of a system**. This becomes unit testing.
2. **Test components of a system individually using static preconditions**. This becomes unit testing.

### **Best Practices**

For more information, reference [https://stackoverflow.com/questions/61400/good-unit-test\#answer-61868](https://stackoverflow.com/questions/61400/good-unit-test#answer-61868). This thread title addresses "unit tests" but the marked answer addresses tests in general, and the following are derived from it.

1. **Thorough**. Test coverage should test all possible states and scenarios.
2. **Repeatable**. Tests should produce the same results every time, and should not be dependent on some auxilliary state or feature.
3. **Professional/Readable**. We have a log of code at Droplit. For the most part, the codebase follows reasonable conventions and is fairly consistent. Tests are no exclusion to this. Intention-revealing names, good comments which discuss what a test is to accomplish, properly indented code, code which role models easy-to-follow procedural logic, etc.

## System Components \(a non-comprehensive list\)

### **Core Components \(Unit Testing\)**

1. dq \(interface library\)
2. redis lock
3. redis multi-lock

### **Component Systems \(Integration Testing\)**

1. API interface
   1. sending commands to edge/cloud/virtual devices
   2. creating/deleting/modifying records
2. Backbone
   1. Emitting device events over the backbone, ultimately to Azure logging system
3. Webhooks
   1. Emitting events that show up in webhooks



