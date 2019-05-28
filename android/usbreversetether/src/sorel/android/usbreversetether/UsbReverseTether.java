package sorel.android.usbreversetether;

import java.lang.reflect.Field;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.os.Bundle;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;

public class UsbReverseTether extends Activity {
	
	private OnItemClickListener mListItemClickedHandler = new OnItemClickListener() {
	    public void onItemClick(android.widget.AdapterView parent, View v, int position, long id) {
	        FieldAdapter fieldAdapter = (FieldAdapter)parent.getAdapter();
	        Field f = fieldAdapter.getField(position);
	        try {
		        Object value = f.get(f.getClass());
		        if (value instanceof String) {
		        	startActivityForResult(new Intent((String)value), 0);
		        } else {
		        	// TODO: show error msg
		        }
	        } catch(IllegalAccessException e) {
	        	// TODO: show error msg
	        }
	    }
	};

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);
		ListView list = (ListView)findViewById(R.id.list_settings);
		Field[] fields = android.provider.Settings.class.getFields();
		FieldAdapter adapter = new FieldAdapter(
                    this, android.R.layout.simple_list_item_activated_1);
		adapter.setFields(fields);
		list.setAdapter(adapter);
		list.setOnItemClickListener(mListItemClickedHandler);
	}

	public void openSettings(View view) {
		startActivityForResult(new Intent(android.provider.Settings.ACTION_SETTINGS), 0);
	}

	public void openWifiSettings(View view) {
		startActivityForResult(new Intent(android.provider.Settings.ACTION_WIFI_SETTINGS), 0);
	}

	public void openWirelessSettings(View view) {
		startActivityForResult(new Intent(android.provider.Settings.ACTION_WIRELESS_SETTINGS), 0);
	}
}

class FieldAdapter extends ArrayAdapter<String> {
	private Field[] mFields;

	public FieldAdapter(Context context, int textViewResourceId) {
		super(context, textViewResourceId);
	}
	
	public void setFields(Field[] fields) {
		mFields = fields;
		for (Field f : mFields) {
			add(format(f.getName()));
		}
	}
	
	public Field getField(int position) {
		return mFields[position];
	}
	
	private String format(String androidSettingsName) {
		Pattern p = Pattern.compile("ACTION_(\\w+)_SETTINGS");
		Matcher m = p.matcher(androidSettingsName);
		if (m.find()) {
			return m.group(1);
		}
		return androidSettingsName;
	}
}
