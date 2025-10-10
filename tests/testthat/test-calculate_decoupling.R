# Load data: sample data from package & mock API returns from helper
data(athlytics_sample_decoupling)
source(test_path("helper-mockdata.R"), local = TRUE) # Provides mock_activity_list_list, mock_activity_streams

# Mock Strava token - needed for function signature but API calls will be mocked 
