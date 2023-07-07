# UnityBasic

A fun hack project to write a BASIC IDE that (hot) compiles to a Unity project. Pushed it for a week to see how far I can go but didn't really reach the point of running a minimal version running end-to-end.

Written in [PureBasic](https://www.purebasic.com).

Has some interesting stuff like live-generating docs from the code and running an embedded HTTP server to serve them to the embedded HTML view :) Also has a live network connection to C# code running in the Unity editor that receives program updates as it is compiled.

The "BASIC" variant is really just a BASIC-ized version of [Flux](https://github.com/Rene-Damm/flux-ml).

That bottom right quadrant in the IDE is meant to show a live view from the standalone Unity player that the C# code running in the Unity editor would continuously update. The project didn't get that far.

![Screenshot](./Screenshot.png)

