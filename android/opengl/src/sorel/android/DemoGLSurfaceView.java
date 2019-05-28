package sorel.android;

import android.opengl.*;
import android.content.Context;

class DemoGLSurfaceView extends GLSurfaceView {

    public DemoGLSurfaceView(Context context) {
        super(context);

        // Create an OpenGL ES 2.0 context
        setEGLContextClientVersion(2);

        // Set the Renderer for drawing on the GLSurfaceView
        setRenderer(new DemoGLRenderer());
        
        // Render the view only when there is a change in the drawing data
        setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY);
    }
}
