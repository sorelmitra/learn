package sorel.android;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.content.Intent;

public class OpenGLActivity extends Activity
{
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
    }

    public void drawTriangle(View view)
    {
        Intent intent = new Intent(this, OpenGLDrawActivity.class);
        startActivity(intent);
    }
}
