package sorel.android;

import android.app.Activity;
import android.os.Bundle;
import android.opengl.GLSurfaceView;

public class OpenGLDrawActivity extends Activity
{
    private GLSurfaceView glView;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);

        glView = new DemoGLSurfaceView(this);
        setContentView(glView);
    }
}
