FROM perl:5.20

RUN curl -L http://cpanmin.us | perl - App::cpanminus
RUN cache=1p5JxG3 git clone https://github.com/hpiili/compareserver.git

RUN cpanm Carton Plack Starman

RUN cd /root/compareserver 
WORKDIR /root/compareserver

CMD carton install --deployment

EXPOSE 5000
CMD carton exec starman -p 5000 hello.psgi


