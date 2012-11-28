// Add Name(s) here
//
// CIS 565 hackathon at the University of Pennsylvania - http://www.seas.upenn.edu/~cis565

#ifdef GL_ES
precision mediump float;
#endif

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;
uniform sampler2D backBuffer;

void main( void ) {
    vec2 rg = gl_FragCoord.xy / resolution; // color components between 0.0 and 1.0
    rg = rg * sin(time);                    // animate over time.  components between -1.0 and 1.0
    rg = abs(rg);                           // components between 0.0 and 1.0
	
    gl_FragColor = vec4(rg, 0.0, 1.0);
}