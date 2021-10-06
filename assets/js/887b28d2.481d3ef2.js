(window.webpackJsonp=window.webpackJsonp||[]).push([[24],{112:function(e,a,t){"use strict";t.d(a,"a",(function(){return i}));var n=t(0),l=t.n(n);function i(e){return l.a.createElement("section",{className:"section "+e.className},e.children)}},115:function(e,a,t){"use strict";t.d(a,"a",(function(){return r}));var n=t(0),l=t.n(n),i=t(113);function r(e){return l.a.createElement("div",{className:"section-image-box__row row "},l.a.createElement("div",{className:"section-image-box__row-text col col--1 left-margin-stub"}),l.a.createElement("div",{className:"section-image-box__row-text col col--5 "},l.a.createElement("div",{className:"section-image-box__row-text-wrapper"},l.a.createElement("h3",null,e.title),l.a.createElement("div",{className:"content"},e.children))),l.a.createElement("div",{className:"section-image-box__row-image col col--6 "},l.a.createElement("div",{className:"section-image-box__row-image-wrapper"},e.image?l.a.createElement("div",{className:"green_border"},l.a.createElement(i.a,{alt:e.image,sources:{light:"img/"+e.image,dark:"img/dark/"+e.image}})):"")),l.a.createElement("div",{className:"section-image-box__row-text col col--1 right-margin-stub"}))}},116:function(e,a,t){"use strict";t.d(a,"a",(function(){return s}));var n=t(0),l=t.n(n),i=t(112),r=t(113);function s(e){return l.a.createElement(i.a,{className:"section-yellow-banner"},l.a.createElement("div",{className:"row row--align-center"},l.a.createElement("div",{className:"col col--6"},l.a.createElement("h1",null,e.title),l.a.createElement("div",{className:"description"},e.children)),l.a.createElement("div",{className:"col col--6"},l.a.createElement("div",{className:"image-wrapper"},l.a.createElement(r.a,{className:"image",alt:e.image,sources:{light:"img/"+e.image,dark:"img/dark/"+e.image}})))))}},117:function(e,a,t){"use strict";t.d(a,"a",(function(){return s}));var n=t(0),l=t.n(n),i=t(115),r=[l.a.createElement(i.a,{image:"versions.svg",title:"Scala versions, dependencies and JVMs",key:"versions",projects:"true"},l.a.createElement("p",null,"Scala CLI is built on top of coursier",l.a.createElement("br",null),"This allow us to manage Scala versions, dependencies and JVMs so you can test your code in different environments by changing single option."),l.a.createElement("p",null,"Scala CLI ships with all its dependencies",l.a.createElement("br",null),"No need to fluff with installing JVM or setting up PATH.")),l.a.createElement(i.a,{image:"universal_tool.svg",title:"Universal tool",key:"universal",projects:"true"},l.a.createElement("p",null,"If you want to use older ",l.a.createElement("b",null,"version of Scala")," or run your code in ",l.a.createElement("b",null,"JS")," or ",l.a.createElement("b",null,"Native")," environments we've got you covered.",l.a.createElement("br",null),l.a.createElement("i",null,"some additional ",l.a.createElement("a",{href:"TODO?"},"setup")," may be required for JS and Native")),l.a.createElement("p",null,"Switching between platforms or Scala versions is as easy as changing a parameter.")),l.a.createElement(i.a,{image:"buildtools.png",title:"We do not call Scala CLI a build tool",key:"buildtool",projects:"true"},l.a.createElement("p",null,"Scala CLI shares some similarities with build tools, but doesn't aim at supporting multi-module projects, nor to be extended via a task system known from sbt, mill or bazel."),",",l.a.createElement("p",null,"Scala ecosystem has multiple amazing build tools, there is no need to create another one.")),l.a.createElement(i.a,{image:"complete-install.svg",title:"Complete installation",key:"complete-install",education:"true"},l.a.createElement("p",null,"Scala CLI comes with batteries included. No additional installation is required, no more fluffing with setting up the correct Java version or ",l.a.createElement("code",null,"PATH")),l.a.createElement("p",null,"Scala CLI manages JVMs, Scala and other used tools under the hood.")),l.a.createElement(i.a,{image:"defaults.svg",title:"Solid defaults",key:"defaults",education:"true"},l.a.createElement("p",null,"No additional configuration is needed to most Scala CLI commands."),l.a.createElement("p",null,"Scala CLI is configured out of the box to use the latest stable versions and other commands such as formatter or compiler contain reasonable defaults.")),l.a.createElement(i.a,{image:"learning_curve.svg",title:"No learning curve",key:"curve",education:"true"},l.a.createElement("p",null,"Scala CLI does not use complex configuration language, its options are simple and self-explanatory"),l.a.createElement("p",null,"There are no big differences in running repl or .scala files so expanding the results of repl session into a small project does not require learning new concepts from Scala CLI perspective")),l.a.createElement(i.a,{image:"powerful_scripts.svg",title:"Scripts are as powerful as other programs",key:"scripts-as-apps",scripting:"true"},l.a.createElement("p",null,"Scripts in Scala CLI can use dependencies and other features as standard Scala programs. Scala CLI is command-line first giving access to all its features without need for any configuration file or specific project structure.")),l.a.createElement(i.a,{image:"embbedable_scripts.svg",title:"Embbedale Scripts",key:"embed-scripts",scripting:"true"},l.a.createElement("p",null,"Scala CLI can be included in shebangs making your .scala or .sc files runnable"),l.a.createElement("p",null,"Scala CLI support piping inputs in and is designed to be embeddable in other scripts turning Scala into proper scripting language")),l.a.createElement(i.a,{image:"fast-scripts.svg",title:"Fast Scripts",key:"fast-scripts",scripting:"true"},l.a.createElement("p",null,"Scala CLI provides multiple ways to reduce the biggest problem of JVM-based scripting solutions: slow start time. Scala CLI aggressively caches inputs removing need for recompilations."),l.a.createElement("p",null,"Scripts can be packaged into the native applications (using e.g. Scala Native) for even faster cold startups.")),l.a.createElement(i.a,{image:"sc-files-support.svg",title:"Support for .sc files",key:"sc-files-support",scripting:"true"},l.a.createElement("p",null,"Scala CLI is backwards compatible with ammonite scripts."),l.a.createElement("p",null,"No need to migrate your existing scripts to use all the powers of Scala CLI.")),l.a.createElement(i.a,{image:"self-contained-examples.svg",title:"Self-contained examples",key:"self-contained-examples",prototyping:"true"},l.a.createElement("p",null,"With Scala CLI, configuration can be included in source code so complex examples can be self-contained and shipped as e.g. gist. Moreover, Scala CLI can compile, run and test gists without any manual work!"),l.a.createElement("p",null,"Scala CLI is a perfect tool to submit and reproduce bugs"))];function s(){return r}},118:function(e,a,t){"use strict";t.d(a,"a",(function(){return i}));var n=t(0),l=t.n(n);function i(e){return l.a.createElement("div",{className:"col col--"+e.colsize},l.a.createElement("h1",{className:"section-title"+(e.promptsign?" with-before":"")},e.title))}},121:function(e,a,t){"use strict";t.d(a,"a",(function(){return m}));var n=t(0),l=t.n(n),i=t(112),r=t(116),s=t(117),c=t(119),o=t(118);function m(e){return l.a.createElement(c.a,{title:e.title,description:e.description,key:e.title},l.a.createElement("div",{className:"container content"},l.a.createElement(r.a,{image:e.image,title:e.headline},e.children),l.a.createElement(o.a,{title:e.title,colsize:"12",promptsign:!0}),l.a.createElement(i.a,{className:"section-image-box"},Object(s.a)().filter((function(a){return a.props[e.id]})))))}},92:function(e,a,t){"use strict";t.r(a);var n=t(0),l=t.n(n),i=t(121);a.default=function(e){return l.a.createElement(i.a,{title:"Scripting with Scala CLI",description:"Page describing why Scala CLI is good for scripting with Scala.",headline:"Scripting using all powers of Scala ecosystem",image:"scripting.svg",id:"scripting"},l.a.createElement("p",null,"Scala-cli allows you to use Scala to create and enhance scripts with using all the goodies of Scala."),l.a.createElement("p",null,"Use dependencies, declare tests or even package your scripts into native applications!"))}}}]);