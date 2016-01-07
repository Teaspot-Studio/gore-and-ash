FROM fedora:23

# Install drivers
RUN dnf install -y mesa-* xorg-x11-drivers 
RUN dnf install -y libXi libXrandr libXcursor libXinerama 
RUN dnf install -y net-tools

# Replace 0 with your user / group id
RUN export uid=1000 gid=1000
RUN mkdir -p /home/developer
RUN echo "developer:x:${uid}:${gid}:Developer,,,:/home/developer:/bin/bash" >> /etc/passwd
RUN echo "developer:x:${uid}:" >> /etc/group
RUN echo "developer ALL=(ALL) NOPASSWD: ALL" >> /etc/sudoers
RUN chmod 0440 /etc/sudoers
RUN chown ${uid}:${gid} -R /home/developer

USER developer
ENV HOME /home/developer

ADD .stack-work/dist/*/*/build/gore-and-ash-client/gore-and-ash-client /home/developer/
WORKDIR /home/developer